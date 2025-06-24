# Delphi 利用 LiveBinding 将对象绑定到界面元素

## 需求
**自己定义的一个类，其对象，如何绑定到界面元素？**

http://docwiki.embarcadero.com/RADStudio/Seattle/en/Tutorial:_Using_TAdapterBindSource_and_the_LiveBindings_Designer

另一篇文章更详细：
http://www.malcolmgroves.com/blog/?p=1084

## 具体实现代码
~~~
比如自己定义了一个类：
 
  TPerson = class
  private
    FMyName: string;
    FMyAge: Integer;
  public
    property MyName: string read FMyName write FMyName;
    property MyAge: Integer read FMyAge write FMyAge;
  end;
~~~

### 开始操作
1. 设计期，拖一个 AdapterBindSource1 到界面上。然后在它的 OnCreateAdapter 事件里面去创建这个对象并绑定，代码如下：    
~~~
procedure TForm1.AdapterBindSource1CreateAdapter(Sender: TObject;
  var ABindSourceAdapter: TBindSourceAdapter);
begin
  FPerson := TPerson.Create;
  FPerson.MyName := 'pcplayer';
  FPerson.MyAge := 28;
 
  ABindSourceAdapter := TObjectBindSourceAdapter<TPerson>.Create(Self, FPerson, True);
end;
~~~
1.1. 需要注意，FPerson := TPerson.Create; 这个必须是在 AdapterBindSource1CreateAdapter 里面。因为这个事件方法执行的时机，比 Form1.OnCreate 更早！  

2. 拖一个 DataGeneratorAdapter1 到界面上，设置 AdapterBindSource1 的 Adapter 属性为这个 DataGeneratorAdapter1 ；  

3. 右键点 DataGeneratorAdapter1 ，下拉菜单选择 Fields Editor。在里面增加字段。增加的字段，定义其字段名为 TPerson 的属性名字，类型为属性的数据类型。  

4. 然后就可以在界面控件的邮件下拉菜单的 Bind Visually 出来的可视化绑定界面里面，看到 DataGeneratorAdapter1  有字段了。然后可以把它的字段连线到界面控件。  

4.1. 弹出这个 Bind Visually 的方法还有：选择一个界面控件，在其 Property 编辑器里面找到 LiveBindings 点下拉。  

5. 上述做法完成后，如果在界面修改了值，要将修改返回到对象，必须调用 AdapterBindSource1.ApplyUpdates 方法；如果程序给对象的属性字段修改了值，要反映到界面上，需要调用 AdapterBindSource1.Refresh.  


**补充**：

1. 在运行期，释放掉原来的那个 FPersong，然后重新创建一个 FPerson := TPerson.Create; 重新给里面的 property 赋值后，调用 AdapterBindSource1.Refresh 方法，没问题。  

2. 同上，如果释放掉 FPerson，然后采用 FPerson := TJSON.JSONtoObject<TPerson>(S); 的方法获得的新的 TPerson  的对象，然后做 AdapterBindSource1.Refresh 会出现 AV 异常。似乎采用 JSON 反序列化回来的对象有点问题。  

3. 同上，如果 TPerson = class(TComponent) 然后从 JSON 反序列化回来， AdapterBindSource1.Refresh 成功，不会有异常。  

4. 同3，在反序列化回来再刷新界面之前，必须对 FPerson 做一次序列化的操作，也就是 TJSON.ObjectToJSONString(FPerson) 的操作。否则同样会出现 AV 异常。这才是整个系统最奇怪的地方。  
