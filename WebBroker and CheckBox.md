# WebBroker and CheckBox

## 需求
在页面上有几个 CheckBox，用户勾选后提交到服务器，服务器端的 WebBroker 程序如何读取到用户的勾选结果？

## 代码
**页面代码如下**：
~~~
<form action="CK" method="post">


    <input type="checkbox" name="CK1" Value="1">跑步
<p></p>
    <input type="checkbox" name="CK2" Value="2">游泳
<p></p>
    <input type="checkbox" name="CK3" Value="3">爬山
<p></p>
    <input type="checkbox" name="CK4" Value="4">打球
    <p></p>
     <input type="submit" value="提交">
</form>
~~~

**服务器端 WebBroker 的代码如下**：
~~~
  for I := 0 to Request.ContentFields.Count -1 do
  begin
    S := S + '; ' + Request.ContentFields.Values[Request.ContentFields.Names[i]];
  end;
~~~

### 代码解释
这里，页面上有 4  个 CheckBox，但 Request.ContentFields.Count 只包括用户勾选了的项目。
因此，如果仅仅是读 Request.ContentFields.Names[i]，就能读到勾选了的 CheckBox 的 name，没勾选的不会读到；

如果页面代码里面，没有 Value ，则读其 Value 只能读到 "on" 。因此，如果每个 CheckBox 没有 Value 值，上述代码只能读到几个 "On"，没法判断用户选择的是哪个。如果要依靠读 Value 来区别用户勾选了哪个，则必须给每个 CheckBox 一个不同的Value。
