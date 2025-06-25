# WebBroker 后端输出页面到浏览器，让浏览器做 URL 跳转
## 需求
使用 WebBroker 做 WEB 服务器，服务器端的代码执行后希望前段网页跳转到另外一个 URL。
## 解决方案
输出一段 JavaScript 代码到前端浏览器，让浏览器执行跳转。
代码如下：
~~~
procedure TWebModule1.WebModule1WebActionLoginAction(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
var
  JS: string;
begin
  JS := '<script language="javascript">window.location.replace("http://localhost/dcgi/testWebBroker.exe/admin");</script>';
    Response.Content := JS; 
end;
~~~

同样的方法，还可以从服务器端输出其它 JavaScript 代码给前端，比如让前端弹提醒框：
~~~
  Reponse.Content := '<script language="javascript">alert("' + 'Hello world' + '");</script>';
~~~
