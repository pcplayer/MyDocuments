# 关于 TaurusTLS 的使用
基于 Indy 的 Web Server 和 WebService，如果要支持 OpenSSL 3.x 则需要使用 TaurusTLS 这个控件。

## 控件下载地址：
https://github.com/JPeterMugaas/TaurusTLS/tree/main 

## 控件安装：

### 官方文档的问题：
它的官方页面里面提到：

1. Set the INDY_PATH environment variable for your user account to the location where Indy is located. 
2. Open TaurusForIndy290All.groupproj in the TaurusTLS\Packages\d12 folder. 
3. Compile TaurusTLS_RTForIndy290. 
4. Compile TaurusTLS_DTForIndy290 and install it in the IDE. 

但实际上，按照上述说法，没法安装。

### 正确的安装方法：

在它的 Packages\d12 文件夹底下，不要使用 TaurusForIndy290All；
找到以下两个包文件，编译，安装。：

**TaurusTLS_RT.dpk**
**TaurusTLS_DT.dpk**

先编译 RT 那个（运行期）；再安装 DT（设计期）。

使用代码：
~~~
procedure TForm1.FormCreate(Sender: TObject);
begin
  FServer := TIdHTTPWebBrokerBridge.Create(Self);
 
  //必须用代码指定。设计期在属性面板里面指定的证书文件，没有效果。
  TaurusTLSServerIOHandler1.DefaultCert.PublicKey := 'mysite.net.cert.pem';
  TaurusTLSServerIOHandler1.DefaultCert.PrivateKey := 'mysite.net.key.pem';
  FServer.OnQuerySSLPort := OnQuerySSLPort;
 
    //写绝对路径也没问题
//  TaurusTLSServerIOHandler1.DefaultCert.PublicKey := 'D:\TestD12\IndySSL_Taurus\证书备份\mysite.net.cert.pem';
//  TaurusTLSServerIOHandler1.DefaultCert.PrivateKey := 'D:\TestD12\IndySSL_Taurus\证书备份\mysite.net.key.pem';
 
  FServer.IOHandler := TaurusTLSServerIOHandler1; 
 
  TaurusTLS.LoadOpenSSLLibrary;
end;
 
procedure TForm1.OnQuerySSLPort(APort: TIdPort; var AUseSSL: Boolean);
begin
  APort := 8080;   //如果不加上这个，就只能走默认的 443
  AUseSSL := True;
end;
~~~

如果证书有密码，需要给它的事件写一个方法：
~~~
procedure TForm1.TaurusTLSServerIOHandler1GetPassword(ASender: TObject;
  var VPassword: string; const AIsWrite: Boolean; var VOk: Boolean);
begin
  VPassword := '';
end;
~~~

### 需要注意的问题
证书文件和 key 文件，都需要用代码指定。设计期属性面板里面填写的，无效。可能是一个 BUG；

## 发布以下4个文件
1. libcrypto-3.dll
2. libssl-3.dll
3. mysite.net.cert.pem
4. mysite.net.key.pem