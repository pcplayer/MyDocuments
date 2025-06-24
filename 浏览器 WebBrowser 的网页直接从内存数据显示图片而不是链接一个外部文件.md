# 浏览器 WebBrowser 的网页直接从内存数据显示图片而不是链接一个外部文件

## 如何让网页里面的图片，直接从内存数据加载，而不是一个图片文件的链接
查了一些资料，基本概念如下：

1. Data URI scheme。这个概念参考：https://en.wikipedia.org/wiki/Data_URI_scheme  

2. Data URI scheme 包括：  
data:,文本数据
data:text/plain,文本数据
data:text/html,HTML代码
data:text/html;base64,base64编码的HTML代码
data:text/css,CSS代码
data:text/css;base64,base64编码的CSS代码
data:text/javascript,Javascript代码
data:text/javascript;base64,base64编码的Javascript代码
data:image/gif;base64,base64编码的gif图片数据
data:image/png;base64,base64编码的png图片数据
data:image/jpeg;base64,base64编码的jpeg图片数据
data:image/x-icon;base64,base64编码的icon图片数据

3. 在 HTML 里面，图片通常是 <img src="http://www.abc.com/abc.jpg"> 这样的链接。这个链接也可以是本地文件。换成 Data URI scheme 的写法：  
<img src="data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAUA
AAAFCAYAAACNbyblAAAAHElEQVQI12P4//8/w38GIAXDIBKE0DHxgljNBAAO
9TXL0Y4OHwAAAABJRU5ErkJggg==" alt="Red dot" />

上面的写法的格式是在 base64 后面，加上了 base64 编码的图片数据。这样就可以直接内嵌图片数据在网页里面，而不是引用外部文件。


用 Delphi 代码来测试上述方法。先写一个 HTML 网页：
~~~
<html>
<head>
<script language="JavaScript">
function setImageData(imageBase64) {
    var myImg = document.getElementById("myImg");
    myImg.src = "data:image/jpeg;base64," + imageBase64;
}
</script>
</head>
<body>
<img id="myImg" src="data:image/jpeg;base64,MyImageDataEncodedInBase64=" alt="My Image data in base 64" />

</body>
<html>
~~~

这个网页里面的 JS 用来对网页里面的 <img id="myImg"...> 填入数据。  

在 **delphi** 里面首先加载这个网页，然后加载一个 JPG 图片并编码为 base64 字符串，并且要去**掉其结尾的回车符**，否则执行加载图片数据的JS时候会出现“未结束的字符串常量”的错误提示。 FMyPicStr := Trim(FMyPicStr);

然后，在 Delphi 里面，调用上述 JS 加载数据。图片显示出来了。测试成功。

调用上述 JS 的方法是调用这个 JS 的函数名。Delphi 代码如下：
~~~
procedure TForm1.Button4Click(Sender: TObject);
var
  doc: IHTMLDocument2;
  S: string;
begin
  //执行 JavaScript 的时候，碰到“未结束的字符串常量”的错误。
  //查，是因为 图片字符串结尾有回车符号。增加 FMyPicStr := Trim(FMyPicStr); 后，问题解决，图片显示出来了。
  doc := WebBrowser1.Document as IHTMLDocument2;

  S := 'setImageData("' + FMyPicStr + '");';
  Memo1.Lines.Text := S;
  doc.parentWindow.execScript(S, 'JavaScript');
end;
~~~

这里是靠拼字符串，把 JS 的函数名称和参数拼到一起的。看网上的 C# 代码，可以不用拼字符串。C#代码如下：
webBrowser1.Document.InvokeScript("setImageData", new[] { imageInBase64 });

但 delphi 里面的 IHTMLDocument2 好像没有 InvokeScript 这个方法。

上述图片显示出来后，还可以通过追加 HTML 字符串的方式，追加一个图片在上面的图片下面，代码如下：
~~~
procedure TForm1.Button5Click(Sender: TObject);
var
  HTDoc: IHTMLDocument2;
  Range: IHTMLTxtRange;
  S: string;
begin
{-----------------------------------------------------------------------------
  写入一段 HTML 文本到浏览器里面。这个文本包括图片及图片数据。以下代码测试成功。
  成功地在上面一个图片的下面，追加了一个图片并显示了出来。
-----------------------------------------------------------------------------}
  S := '<img src="data:image/jpeg;base64,' + Self.FMyPicStr + '">';
 
 
  HTDoc := WebBrowser1.Document as IHTMLDocument2;
 
 
  if (HTDoc <> nil) then
  begin
    Range := (HTDoc.body AS IHTMLBodyElement).createTextRange;
    Range.Collapse(False);
    Range.PasteHTML(S);
  end;
end;
~~~

## 又及
上述的 Delphi 代码是调用 IHTMLDocument2 操作 TWebBrowser 这个 Windows 里面基于 IE 的浏览器控件。
现在如果在 Windows 底下，应该使用 TEdgeBrowser 这个更现代的浏览器控件。这个控件如何执行 JS 代码，需要别的方法。