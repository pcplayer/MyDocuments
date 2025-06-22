# Delphi程序响应 TEdgeBrowser 页面里面的点击
## 需求
使用 Delphi VCL 开发的程序，Form 上面放一个 TEdgeBrowser 用来呈现 HTML 页面。用户在页面里面点击了某个元素，Delphi 程序需要能够响应。

## 实现方法
首先是简单的测试页面代码，页面里面使用一个 JavaScript 来响应用户点击并向 Delphi 程序发送消息，页面代码如下：
~~~
<!DOCTYPE html>
<html>
<body>
    <p onclick="handleClick()">Click me</p>
    <script>
        function handleClick() {
            window.chrome.webview.postMessage({data: 'Message from Edge Chromium 哈哈', url: window.document.URL });
        }
    </script>
</body>
</html> 
~~~
把这个页面保存为文件：test.html，方便 Delphi 程序加载页面。

把这个 test.html 程序放到 Delphi 程序的 EXE 目录下。

加载页面到 TEdgeBrowser 的Delphi 程序代码如下：
~~~
procedure TForm1.Button1Click(Sender: TObject);
begin
  EdgeBrowser1.Navigate(ExtractFilePath(ParamStr(0))  + 'test.html');
end;
~~~
执行上述代码后，可以看到页面出来了，就显示了一行字：Click me

当你的鼠标点击它，会触发 TEdgeBrowser 的一个事件，在 Delphi 里面写事件代码，就可以获得用户点击的消息。代码如下：
~~~
procedure TForm1.EdgeBrowser1WebMessageReceived(Sender: TCustomEdgeBrowser;
  Args: TWebMessageReceivedEventArgs);
var
  Msg: PChar;
begin
  //页面内部的 JS 执行后发送的消息由这里来响应
  Args.ArgsInterface.Get_webMessageAsJson(Msg);
  MessageBox(Handle, Msg, PChar(EmptyStr), MB_OK);
end;
~~~

#搞定！