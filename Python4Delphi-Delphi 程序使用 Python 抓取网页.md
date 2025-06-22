# Python4Delphi: Delphi 程序使用 Python 抓取网页
想用程序去抓取一个网页的内容，Delphi 有自己的 HTTP 库。比如 Indy 的 TIdHTTP，或者 TNetHTTPClient。

这里测试一下使用 Python 的 HTTP 库抓取网页，然后把抓取的内容给 Delphi 的程序。

## Delphi 程序
界面上拖控件如下：
~~~
    Panel1: TPanel;
    Button1: TButton;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    Memo1: TMemo;
    Splitter1: TSplitter;
    Memo2: TMemo;
    PythonEngine1: TPythonEngine;
    PythonDelphiVar1: TPythonDelphiVar;
    EdgeBrowser1: TEdgeBrowser;
    Button2: TButton;
    PythonGUIInputOutput1: TPythonGUIInputOutput;
~~~
### 上述控件的主要设置：
1. PythonEngine1.IO := PythonGUIInputOutput1;

2. PythonGUIInputOutput1.Output := Memo1;  //用 Memo1 来显示 Python 代码打印出来的网页内容。

3. PythonDelphiVar1.Engine := PythonEngine1;

4. PythonDelphiVar1.VarName := 'MyHTML'; //这个变量名称，会在 Python 代码里面使用。

5. 在 Memo2.Lines 里面放 Python 代码。

6. EdgeBrowser1 用于显示由 Python 抓回来的页面内容。

### Delphi 的主要代码如下：
~~~
procedure TForm3.Button1Click(Sender: TObject);
var
  i: Integer;
begin
  EdgeBrowser1.Navigate('D:\test.html');
 
  //循环等待 EdgeBrowser1 打开完成。否则当 Python 抓取到页面后写入 EdgeBrowser 会失败。
  i := 0;
  while True do
  begin
    Sleep(100);
    Inc(i);
    Application.ProcessMessages;
    if i > 10 then Break;
  end;
 
  PythonEngine1.ExecStrings(Memo2.Lines);
end;
 
procedure TForm3.Button2Click(Sender: TObject);
var
  S: string;
begin
//  S := '<html><head></head><body>abc 12333 <p> hello world!!!</body></html>';
  S := VarToStr(PythonDelphiVar1.Value);
  ShowHTML(S);
end;
 
procedure TForm3.PythonDelphiVar1SetData(Sender: TObject; Data: Variant);
var
  S: string;
begin
  //ShowMessage('写页面');
  S :=  VarToStr(Data);
  ShowHTML(S);
end;
 
procedure TForm3.ShowHTML(const S: string);
begin
{--------------------------------------------------------------------------
  使用 EdgeBrowse 必须：
  1. 当前目录下有 WebView2Loader.dll
  2. 必须先 Navigate 打开后，才能 NavigateToString
--------------------------------------------------------------------------}
  EdgeBrowser1.NavigateToString(s);
 
end;
~~~

### Python 代码如下：
~~~
import http.client
 
# 页面的路径：blog.csdn.net/pcplayer
 
conn = http.client.HTTPSConnection("blog.csdn.net")  # 工作正常，这里只能填写站点名称。子路径在 request 里面填写。
 
conn.request("GET", "/pcplayer")
response = conn.getresponse()
print(response.status, response.reason)
data = response.read()
print(data.decode())  # 这里是输出的网页内容
 
# MyHTML 是 Delphi 的控件对应的变量，它被赋值时会在 Delphi 代码里触发 OnSetData 事件。
# Delphi 程序在这个 OnSetData 事件里面拿到页面内容，写到浏览器里面去，让浏览器显示。
MyHTML.value = data.decode()
 
# print 把页面内容输出，这个输出在 Delphi 程序里面会显示到 Memo1 里面。
print(response.url)
conn.close()
~~~

### 简单解释：
1. Delphi 程序里面的 EdgeBrowser 加载一个 test.html 页面。加载这个页面是为了打开这个 Browser 方便后继写入抓取的页面内容。

2. PythonEngine1 执行 Memo2 里面的 Python 代码；

3. PythonGUIInputOutput1 将 Python 代码执行后的 print 语句的内容输出到 Memo1 里面。这里是程序抓取到的网页内容。

4. PythonDelphiVar1 获取到 Python 代码里面的 MyHTML.Value 的值，也就是页面内容。并将这个内容写入 EdgeBrowser 浏览器显示。

# 总结：
在 Delphi 里面通过使用 Python4Delphi 控件，可以在 Delphi 程序里面调用 Python 的代码去执行 HTTP 访问。