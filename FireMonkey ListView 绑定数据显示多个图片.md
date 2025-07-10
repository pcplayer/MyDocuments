# FireMonkey ListView 绑定数据显示多个图片

## 概述
采用 Delphi 的 FireMonkey 框架，可以开发运行在 WINDOWS, Android, iOS 和 MacOS 上面的代码。其中一个比较常用的界面显示控件是 TListView。

如果数据是多条，来自数据库，或者来自多个对象的对象列表（TObjectList），采用 Delphi 的数据绑定功能（LiveBindings）可以让 ListView 显示来自数据的内容。

## 需求：
ListView 显示的多条内容，每一条，可以有一个图片，有段文字，等等。Delphi 提供了几种固定格式。但如果在一条里面，想要显示多个图片，怎么办？而且，这些图片，不想要复制拷贝图片，而是直接使用索引指向一个 ImageList 里面的图片。

## 实现：
在 Delphi IDE 里面，鼠标右键点 ListView1，下拉菜单，选择设计模式，使得 ListView1 进入设计模式。这个时候选择界面上 ListView1 的设计模式下的 Item，在 IDE 右上的【Structure】窗口里面，找到对应的 Item 项目，选中，左边的属性窗口里面，就是这个 Item 的属性，属性的第一条【Appearance】，下拉选择【DynamicAppearance】，这个 Item 就可以在设计期手动增加显示用的元素了，这个时候属性里面的最后一条显示的是【+】号和【Add new】，点击它最右边的下拉箭头，选择 TImageObjectAppearance 就可以增加一个图片元素到 Item 上面。

想要增加一条文字，也同样操作，选择 TTextObjectAppearance 就可以了。

这些设计期手动增加的界面元素，可以修改它的名字。默认的名字大概是 Text1, Image1 这样的。

**如果想要用代码为这个图片元素赋值，可以找到对应的 Item 然后采用以下代码**：
~~~
procedure TForm594.SpeedButton1Click(Sender: TObject);
var
  LItem: TListViewItem;
begin
  LItem := ListViewl.Items[3];
  (LItem.Objects.FindDrawable('Image1') as TListItemImage).Bitmap := MyImage.Bitmap;
end;
 
//备注：MyImage 是一个 TImage，我事先在里面加载了图片。
~~~

**如果要让某一条 Item 的图片按照一个 ImageList 的序号显示对应图片，以下代码**：
~~~
//备注：ListView1.Images := ImageList1;
 
procedure TForm594.SpeedButton1Click(Sender: TObject);
var
  LItem: TListViewItem;
begin
  LItem := ListView1.Items[3];
  
  LItem.Data['Image1'] := 12; //12 是对应的图片在 ImageList1 里面的序号。
end;
~~~

如果采用 **LiveBindings** 的方法，设计期可视化绑定操作时，只需要把要绑定的对象的整数属性拉线到这个 Image1 元素上，它就可以显示对应的 ImageList1 里面的图片。

绑定到数据库的 DataSet 上，道理相同。