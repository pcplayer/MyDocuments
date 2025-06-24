Delphi WebBroker 上传文件

## 需求
使用 Delphi WebBroker 做一个 WEB 页面，在页面里可以上传文件给服务器端程序接收。

## 代码

### 网页代码
在浏览器页面里面上传文件，就是有一个上传文件的按钮，用户点击后，浏览器会弹出一个打开文件对话框。用户在对话框里面选择文件后，浏览器开始执行文件上传。  
要让浏览器实现这个，需要在网页代码里写如下代码：
~~~
      <form action="../testWebBroker.exe/upload" method="post" enctype="multipart/form-data">
          <input type="file" id="MyUpload" name="MyUpload">
          <input value="submit" type="submit">
      </form>
~~~
**代码解释**：
上述代码里面，action 是指这个页面 form 的按钮被用户点击后要执行的服务器端路径。这里的路径指向了本文的 WebBroker 程序 testWebBroker.exe，后面的 upload 则是 WebBroker 里面的一个 Action。  

**注意**：<input type="file" > 这样在服务器端是看不到文件的。必须要为这个元素加上 name="MyUpload" 才行！比如：<input type="file" name="file1">。因为在服务器端的 WebBroker 里面依靠 Name 来读取客户端提交的内容。  
**注意**：页面代码里面，一定要有 **enctype="multipart/form-data"**，如果没有，在服务器端的 WebBroker 收不到文件。   

### WebBroker 的 Delphi 代码
首先，在 WebBroker 的代码里面，一定要  **uses WEB.ReqMulti**;
如果一个网页的 Form 里面要上传多个文件，则需要用到 **WEB.ReqMulti** 单元的 **TMultipartContentParser**

服务器端 Delphi WebBroker 的代码如下
~~~
   FFileName := '文件名' + Request.Files[0].FileName;
 
  AFileName := ExtractFileName(Request.Files[0].FileName);
  AFileName := ExtractFilePath(GetModuleName(0)) + AFileName;
  AFile := TFileStream.Create(AFileName, fmCreate);
  try
    Request.Files[0].Stream.Position := 0;
    AFile.CopyFrom(Request.Files[0].Stream, Request.Files[0].Stream.Size);  //测试保存文件，通过。
  finally
    AFile.Free;
  end;
 
  FFileName := HTMLEncode(FFileName);
 
  PageProducerUpload.HTMLFile := 'FileUpload.htm';     //设计期不要指定 PageProducer 的 HTML 模板文件，而是在这里指定。如果在设计期指定，则 PageProducer 的 OnHTMLTag 事件会先于本路径事件的执行，则新页面无法显示上传的文件名。
  Response.Content := PageProducerUpload.Content;
~~~

**代码解释**：
上述代码，是执行在 WebBroker 里面，我增加的一个 Name 叫做 upload 的 Action 的事件里面。当页面上用户点击了文件上传，会在服务器端触发 upload 这个 Action 的事件，在这个事件里面，上述代码被执行。  
另：WebBroker 的 Action 你可以取任意的名字，不一定是 upload，当然，如果这个名字变了，页面代码里面对应的名字也要变。

### 以下是TMultipartContentParser的官方帮助文档
http://docs.embarcadero.com/products/rad_studio/delphiAndcpp2009/HelpUpdate2/EN/html/delphivclwin32/ReqMulti_TMultipartContentParser.html


Web request objects use TMultipartContentParser object to parse the content of an HTTP request message from a multipart form when that request may contain uploaded files. The Web request uses this content parser to assign values for its ContentFields and Files properties. 


TMultipartContentParser is only used when the current HTTP request object has a content type of 'multipart/form-data'. Multipart forms built using a WebSnap application automatically use this content type. 


To allow your WebSnap application to use TMultipartContentParser in Delphi, add the ReqMulti unit to the end of your project uses clause. In C++, include ReqMulti.hpp in the header of your project source file. 

**一个想法**: 看起来，似乎我们在其它地方需要分析一个 MIME 文档的 multipart form 的时候，也可以用这个TMultipartContentParser。

## Done.