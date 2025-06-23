# Delphi VCL 的 TScrollBox 响应鼠标滚轮的问题

## 问题：ScrollBox 如何在鼠标指上去的时候，鼠标滚轮滚动，它就跟随卷动？

测试环境：Delphi 11 CE version + Windows 11； 其它版本的 Delphi 不一定相同。

### 测试方法
首先，创建一个用于测试的简单的 VCL 程序，有一个 Form1。

然后，拖一个 ScrollBox1 到 Form1 上面，置顶。底下留一些 Form1 出来方便放一个 Label 用于显示滚动时的数据。

在 ScrollBox1 上面从上到下摆一些控件，比如 Edit1, Button1, Memo1 等等。上下的摆放超出 ScrollBox1 的高度，因此，它就会出现垂直滚动条。

按 F9 编译运行。程序运行起来，把鼠标移动到 ScrollBox1 上面，滚动鼠标的中间那个滚轮，发现 ScrollBox1 没有卷动。拖动 ScrollBox1 右侧的垂直滚动条，ScrollBox1 会上下卷动。

#### 如何让它响应鼠标滚轮
在 IDE 里面选择 ScrollBox1，查看属性面板，切换属性面板到 Event 查看它有什么事件，发现它有一个 OnMouseWheel 事件。就用它了。双击属性面板上的这个事件，Delphi IDE 自动创建事件代码框架如下：
~~~
procedure TForm1.ScrollBox1MouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
 
end;
~~~

试着在这里面添加代码：
~~~
procedure TForm1.ScrollBox1MouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  ScrollBox1.VertScrollBar.Position := ScrollBox1.VertScrollBar.Position - WheelDelta;
  ScrollBox1.ScrollBy(0, ScrollBox1.VertScrollBar.Position);  
end;
~~~

再次编译运行。然后鼠标移动到 ScrollBox1 上面，滚动鼠标滚轮，ScrollBox1 确实随着鼠标卷动起来了。

并且，焦点在 Form1 上的另外一个 Edit1 的时候，它同样能滚动。也就是 ScrollBox1 不需要有焦点才滚动。

#### 但是，问题没那么简单
反复测试，发现几个问题：

1. 鼠标滚轮滚一格，ScrollBox1 的滚动跑好远。不够平滑细腻。虽然，我已经设置了 ScrollBox1 的垂直滚动条的 Smooth 属性为 True。

2. 如果是拖动滚动条，拖到顶部，就不能再往上拖了。但上述滚动代码，鼠标滚轮继续往上滚，ScrollBox1 的滚动条也继续往上，里面的内容也继续往下，一直往下，内容都完全跑出显示区域了还可以继续滚动。滚轮往下滚，内容往上走，一直到内容的最底部都往上走出显示区域了，还可以继续滚动。显然不符合正常使用习惯。正常情况应该是滚到显示内容的顶，或者滚到显示内容的底部，就不再继续滚动。

### 上述问题的解决
1. 仔细观察发现 WheelDelta 的值很大。我的鼠标滚轮手感上有一格一格的感受，滚动一格，这个 WheelDelta 的值太大，导致上述代码里面采用它作为响应滚动事件的滚动偏移值太大。因此，必须另外搞一个滚动步进值。

2. 拖动 ScrollBox1 右侧垂直滚动条上下走，ScrollBox1.VertScrollBar.Position 值会变化，拖到顶它变为 0；但是，如果是鼠标滚动轮导致的滚动，这个值没变化，始终是初始化的 0；因此，无法用它作为滚动到顶或者滚动到底的判断，导致可以一直滚动把显示内容都滚动出显示区域。因此，这里需要自己定义一个滚动位置的变量，而不能依靠 ScrollBox1.VertScrollBar.Position 这个值。

新的代码如下：
~~~
procedure TForm1.ScrollBox1MouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
var
  GoStep: Integer;
begin
  Label1.Caption := WheelDelta.ToString;
  Self.Caption := FPosition.ToString;  //显示当前位置数据，调试用。
 
  if ((FPosition <= 0) and (WheelDelta > 0)) then Exit;
 
  if (FPosition >= (Panel2.Top )) and (WheelDelta < 0) then Exit; //Panel2 是最底下的控件。
 
  if WheelDelta > 0 then GoStep := 10 else GoStep := -10;
  ScrollBox1.ScrollBy(0, GoStep);
  FPosition := FPosition - GoStep;
  Handled := True;
end;
~~~
上述代码中，Panel2 是放在 ScrollBox1 中，从上到下的布局中，最底下的控件。因此，它露出来，就等于滚动到最底部了。用它来做判断，停止继续往下滚动。

### 还有个问题：
ScrollBox1 里面摆放了一个 Memo1 或者 DBGrid 这样的本身就可以滚动的控件，怎么办？

DBGrid 我没有测试。我放了一个 Memo1 在里面，在这个 Memo1 里面放了很多行内容，设置了它的垂直滚动条显示属性为 True；

然后运行程序，鼠标移动到这个 Memo1 上面，滚动鼠标的滚轮，发现：

1. Memo1 里面的内容在滚动；

2. ScrollBox1 也在滚动。

这样不符合正常操作的直觉。

正常操作应该是：

1. 鼠标悬浮在 ScrollBox1 上面，但没有在 Memo1 上面，这个时候鼠标滚动，ScrollBox1 就滚动；

2. 鼠标如果悬浮在 Memo1 上面，鼠标滚动，则 Memo1 滚动，此时 ScrollBox1 不能滚动。

上述情况，类似浏览器的右侧上下滚动条，当浏览器内部页面上有一个内容滚动条时，鼠标指向内部的内容的滚动条，则浏览器页面的滚动条就不应该滚动。

#### 解决方案
增加一个 ScrollBox1 是否需要滚动的变量。此变量由 Memo1 的鼠标进入/离开事件来改变。

完整代码如下：
~~~
procedure TForm1.Memo1MouseLeave(Sender: TObject);
begin
  FCanScroll := True;  //鼠标在 Memo1 上面，则鼠标滚轮动作， ScrollBox 不滚动，而是 Memo1 滚动。鼠标离开 Memo1 则 ScrollBox1 滚动。
end;
 
procedure TForm1.ScrollBox1MouseEnter(Sender: TObject);
begin
  FCanScroll := True;
end;
 
procedure TForm1.ScrollBox1MouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
var
  GoStep: Integer;
begin
  if not FCanScroll then Exit;
 
  Label1.Caption := WheelDelta.ToString;
  Self.Caption := FPosition.ToString;
 
  if ((FPosition <= 0) and (WheelDelta > 0)) then Exit;
 
  if (FPosition >= (Panel2.Top )) and (WheelDelta < 0) then Exit; //Panel2 是最底下的控件。
 
  if WheelDelta > 0 then GoStep := 10 else GoStep := -10;
  ScrollBox1.ScrollBy(0, GoStep);
  FPosition := FPosition - GoStep;
  Handled := True;
end;
~~~

## 结束语
到这里，基本的功能已经实现，ScrollBox1 已经可以正常响应鼠标的滚轮。代码基于 Delphi 本身控件的事件方法，没有去拦截系统的滚轮消息等。当然，拦截鼠标滚动消息然后写一堆复杂代码，可能通用性更强。但本文的做法，也是一个简单的解决方案。