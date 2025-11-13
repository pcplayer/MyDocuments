# WebService 程序开发经验总结

## 登录/身份验证
最简单的办法是使用 Cookie；要使用 Cookie，在服务器端，首先需要：
~~~
uses WebBrokerSOAP, HttpApp
~~~

然后，在服务器端的 SOAP 接口实现代码里面：
~~~
function TCMSData.Login(const UserId, PassWord: string): Boolean;
var
  DM: TDmCMSFb;
  ACookie: TCookie;
begin
  DM := TDmCMSFb.Create(nil);
  try
    Result := Dm.Login(UserId, Password);
  finally
    DM.Free;
  end;

  if not Result then Exit;

  ACookie := GetSOAPWebModule.Response.Cookies.Add;
  ACookie.Name := 'MyCookie';
  ACookie.Value := 'abc';
  ACookie.Expires := Now + 1;
end;
~~~

上述代码，就是客户端登录时，返回 Cookie 给客户端。
当客户端登录成功，获得 Cookie 后，客户端下次访问，**服务器端**需要检查 Cookie，则可以使用以下方法：
~~~
function TCmsDataWS.CheckCookie: Boolean;
begin
  Result := GetSOAPWebModule.Request.CookieFields.Values['MyCookie'] = 'abc';
end;
~~~
上面的 GetSOAPWebModule 同样需要 uses **Soap.WebBrokerSOAP**;

**服务器端 Session**
服务器端需要把 Cookie 随机数写入一个和用户ID对应的表，方便检查核验用户访问时从 Request 读到的 Cookie。此表记录上次访问时间，方便检查超时，而不是依靠 HTTP Cookie 本身的超时设置。

### 客户端多个不同的 HTTPRIO 和 SoapConnection 共享 Cookie
调用服务器端的接口方法，需要使用一个 HttpRIO，使用 ClientDataSet 直接向服务器端的 DataSetProvider 读取数据/提交数据，则需要 SoapConnection。
在 Delphi 10.3 以后的版本中，多个 HttpRIO 和多个 SoapConnection 没有共享 Cookie，因此使用 HttpRIO 去执行 Login 获得的 cookie，在使用 SoapConnection 去读取数据时，就没有向服务器端提交 HttpRIO 获得的 Cookie。
要共享 Cookie，需要这样做：
~~~
uses System.Net.HttpClient, Soap.SOAPHTTPTrans;  
~~~
定义一个全局的 Cookie 对象：
~~~
FCookieManager: TCookieManager;
~~~

然后，把几个 RIO 的 Cookie 都指向这个对象：
~~~
procedure TDmCmsClient.DataModuleCreate(Sender: TObject);
begin
  SoapConnection1.Connected := False;
  FCookieManager := TCookieManager.Create;
  HTTPRIO1.HTTPWebNode.HTTP.CookieManager := Self.FCookieManager;
  SoapConnection1.Connected := True;
  SoapConnection1.RIO.HTTPWebNode.HTTP.CookieManager := Self.FCookieManager;
end;
~~~

**关于 URL** 上述代码中，如果 SoapConnection1 重新设置了 URL，则需要重新设置其 CookieManager；

### 服务器端对于读写数据的登录验证
客户端读写数据，如果需要进行登录验证，只能在对应的 DataSetProvider 的 BeforeGetRecores 和 BeforeApplyUpdates 事件里面进行登录检查验证。
如果直接在 TSoapDataModule 的 OnCreate 事件里面验证没有登录 Cookie 的直接抛出异常，不会打断从 DataSetProvider 读取数据。
因此，必须给每个 DataSetProvider 对应的事件里面写检查登录代码。
如果给每个 DataSetProvider 都双击其事件，产生事件方法代码框架，在里面写登录验证，就会带来太多的重复代码。如果把它们都指向同一个事件方法，则少写很多代码。
#### 少写登录验证代码的方法
都指向同一个事件方法的办法有 2 个：
1. 设计期在事件面板里面下拉指向； 
2. 用代码来绑定。 
设计期指定的方式，可能会漏掉。而且如果 DataSetProvider 多了，每个都去设置一下，也很费事。
用代码方式：

~~~
procedure TCmsDataWS.DspUserBeforeApplyUpdates(Sender: TObject;
  var OwnerData: OleVariant);
begin
  if not Self.FLogined then raise Exception.Create('请先登录！');
end;

procedure TCmsDataWS.DspUserBeforeGetRecords(Sender: TObject;
  var OwnerData: OleVariant);
begin
  if not Self.FLogined then raise Exception.Create('请先登录！');
end;
~~~

上述代码里面的 FLogined 是在数据模块创建时检查 Cookie 获得的：
~~~
procedure TCmsDataWS.SoapDataModuleCreate(Sender: TObject);
var
  i: Integer;
begin
  FLogined := Self.CheckCookie; 
  
  for i := 0 to Self.ComponentCount -1 do
  begin
    if (Self.Components[i] is TDataSetProvider) then
    begin
      TDataSetProvider(Self.Components[i]).BeforeGetRecords := DspUserBeforeGetRecords;
      TDataSetProvider(Self.Components[i]).BeforeApplyUpdates := DspUserBeforeApplyUpdates;
    end;
  end;
end;
~~~

#### 上述方法的问题及解决方案
在 DspUserBeforeGetRecords 方法和 DspUserBeforeApplyUpdates 方法里面，OwnerData 是用于客户端的 ClientDataSet 在读/写数据时，和服务器端通讯，提供额外数据用的。
在业务上，如果 ClientDataSet 需要根据不同的业务状况提供不同的数据，就不能所有的 DataSetProvider 的上述两个事件都指向同一个事件方法，因为没法区别处理 OwnerData；
在某些业务场景下，为了方便同一个 ClientDataSet 根据不同的状况，需要服务器端有不同的 SQL 语句或者不同的参数，使用 OwnerData 向服务器端传递状态和参数。如果需要少写登录验证代码，在这种业务需求时，需要设计期使用不同的 DataSetProvider 来对应不同的 SQL 语句和参数，然后在客户端根据需要，动态切换 ClientDataSet 指向的 DataSetProvider 的名字。

### 服务器端多 SoapDataModule 模块架构
服务器端的 TSoapDataModule 可能有多个；接口和接口实现函数也可能有多个，需要用到数据库的 DataSet；
因此，需要把数据库连接和 DataSet 都拿出来，放到一个单独的 DataModule 里面。然后在接口实现代码里面，动态创建这个 DataModule；在 TSoapDataModule 里面，也动态创建这个 DataModule；然后 TSoapDataModule 里面只放 DataSetProvider，用代码把它的 DataSet 指向这个额外的 DataModule 里面的 DataSet；
例子代码如下：
~~~
procedure TCmsDataWS.SoapDataModuleCreate(Sender: TObject);
begin
  FDmCMSFb := TDmCMSFb.Create(Self); //TDmCMSFb 在这里被复用。
  DspUser.DataSet := FDmCMSFb.FdqUser;
end;
~~~
接口实现里面重复使用这个 DataModule **TDmCMSFb**:
~~~
function TCMSData.Login(const UserId, PassWord: string): Boolean;
var
  DM: TDmCMSFb;
  ACookie: TCookie;
begin
  DM := TDmCMSFb.Create(nil); //TDmCMSFb 在这里被复用。
  try
    Result := Dm.Login(UserId, Password);
  finally
    DM.Free;
  end;

  if not Result then Exit;

  ACookie := GetSOAPWebModule.Response.Cookies.Add;
  ACookie.Name := 'MyCookie';
  ACookie.Value := 'abc';
  ACookie.Expires := Now + 1;
end;
~~~

当然，一部分仅仅是 TSoapDataModule 里面才用的 DataSet，也可以直接放到这个 TSoapDataModule 里面。

这里，Login 函数，不要放到 TSoapDataModule 的接口方法里面去。最好就是使用单独的接口以及接口实现。那么，就需要单独的 DataModule 模块来使用 DataSet 去处理 Login 函数和数据库有关的东西。

**上述方法中 TDmCMSFb 的创建**：上面的 Login 是在 Login 函数里面创建。如果这里的接口方法有很多个，每个方法里面都写创建代码，则代码重复了。这里应该抽取出来，给 TCMSData 一个属性，该属性的 Get 方法里面创建 TDmCMSFb。那么，就必须要给 TCMSData 增加一个 Destroy 方法，在这里释放 TDmCMSFb。

### 服务器端多个 TSoapDataModule 模块
如果数据库的表非常多，则需要分开到多个 TSoapDataModule 里面去，避免一个单元的代码太多。对于服务器端来说，有多个 TSoapDataModule 不是问题。问题是客户端如何让不同的 ClientDataSet 对应到服务器端不同的 TSoapDataModule 模块里面去。
在客户端稍微麻烦一点：
https://blog.csdn.net/pcplayer/article/details/110178123

#### 多个 SoapDataModule 模块的总结
1. 服务器端可以有多个 SoapDataModule;

2. 客户端的 SoapConnection 要访问哪个 SoapDataModule，则设置其对应的 IID；SoapConnection.URL := 'http://127.0.0.1:8080/soap';  这个 URL 不变。变的仅仅是 SoapConnection1.SOAPServerIID

3. 上述 IID 是客户端对应的服务器端 SoapDataModule 里面的接口的 IID；在客户端，这个 IID 有两种来源方式，取一种就可以了；

3.1. 服务器端运行起来，客户端通过 Delphi IDE 提供的菜单里面的 Import WSDL 菜单提供的功能，从服务器获得接口定义文档，Delphi 自动创建对应的 pas 源代码文件。这是一种方式，尤其是当服务器端是其它语言比如 JAVA 开发的时候，用 Delphi 开发客户端，需要通过这种方式获得服务器端的接口声明。上面说的 IID 就在这个接口声明文件里面。

3.2. 服务器端是 Delphi 开发的，客户端可以直接引用服务器端的接口声明文件，使用服务器端接口声明文件里面给该接口的 IID；
3.2.1. 这里的 IID 指 SoapDataModule 里面实现的那个接口的 IID；这个接口是声明在 SoapDataModule 里面的。
3.2.2. 把上述接口声明挪到一个独立的文件里面，这样方便客户端直接引用接口文件。
3.2.3. 客户端如果直接引用这个接口文件，则客户端就可以直接使用这个 IID 作为 SoapConnection 的 IID；这样客户端就能在服务器端有多个 SoapDataModule 的情况下，对应到正确的 SoapDataModule；
3.2.4. 接口声明挪到单独的接口声明单元，初始化部分注册此接口的代码也要挪到此单元。

3.3. 这里有点奇怪的是，3.1 生成的接口声明里面的 IID 和 3.2 里面提到的服务器端的接口声明源代码的 IID 并不相同。但是，经过测试，同样有效。

客户端要连接不同的服务器端模块，就给与对应该模块的 GUID，代码：
~~~
procedure TForm1.Button6Click(Sender: TObject);
begin
  //测试服务器端有多个 SoapDataModule 的情况是否可以在客户端调用
  SoapConnection1.Connected := False;
  SoapConnection1.SOAPServerIID := '{52B5735F-378C-427E-8590-19C4704FAC94}'; // '{C99F4735-D6D2-495C-8CA2-E53E5A439E61}'; // '{F28A698B-98B0-DF8E-4977-6AE075489E81}';
  SoapConnection1.URL := 'http://localhost:8080/soap';
  SoapConnection1.Connected := True;
  ClientDataSet3.Open;
end;
~~~

#### 设计期，客户端能看到哪个 SoapDataModule
服务器端有多个模块的情况下，运行服务器端。此时，设计期的客户端的 ClientDataSet 通过 SoapConnection 去连接服务器端，下拉 DataSetProvider 属性，只能看到一个模块。设计期改动 SoapConnection 的 GUID 也不能看到服务器端其它的模块。 
客户端看到的那个，就是服务器端程序的工程项目源代码的 uses 里面排前面的那个。

**需要注意的问题**
如果因为某种原因，SoapDataModule 之间互相引用或者通过第三方模块产生了引用，则会导致不管如何调整 uses 里面的顺序，设计期客户端看到的 SoapDataModule 都会是被引用的那个，因为那个必须首先加载。

## WebService 服务器端对 SSL / TLS / https 的支持

基于 Indy 内置 WebServer 的 SOAP Server 如果要支持 https 方式，则必须通过加载 OpenSSL 来实现。
如果直接编译为 CGI/ISAPI 则由 WEB SERVER（IIS）来负责。

### 使用 OpenSSL 1.x 库
使用 OpenSSL 1.x 库，是 Indy 默认支持的。方法是增加一个 IOHandler 控件去加载 OpenSSL 的 dll 库文件，以及加载 key私钥文件和证书文件。
代码如下：
~~~
procedure TForm1.LoadPem;
begin
  //加载证书
  IdServerIOHandlerSSLOpenSSL1.SSLOptions.CertFile := TPath.Combine(Self.GetMyPath, 'mis.mysite.net.cert.pem'); 
  IdServerIOHandlerSSLOpenSSL1.SSLOptions.KeyFile := TPath.Combine(Self.GetMyPath, 'mis.mysite.net.key.pem'); 
  IdServerIOHandlerSSLOpenSSL1.SSLOptions.RootCertFile := TPath.Combine(Self.GetMyPath, 'ca.cert.pem'); 
  IdServerIOHandlerSSLOpenSSL1.SSLOptions.Mode := sslmServer;
  IdServerIOHandlerSSLOpenSSL1.SSLOptions.VerifyMode := [];
  IdServerIOHandlerSSLOpenSSL1.SSLOptions.VerifyDepth  := 0;
  IdServerIOHandlerSSLOpenSSL1.SSLOptions.SSLVersions := [sslvTLSv1_2]; 
  //IdServerIOHandlerSSLOpenSSL1.OnGetPassword := GetPassword;
end;

procedure TForm1.StartServer;
begin
  if not FServer.Active then
  begin
    LoadPem;
    FServer.Bindings.Clear;
    FServer.DefaultPort := StrToInt(EditPort.Text);
    FServer.IOHandler := IdServerIOHandlerSSLOpenSSL1;
	//D12 下，必须要新增：
	FServer.OnQuerySSLPort := OnQuerySSLPort;
    FServer.Active := True;
  end;
end; 

procedure TForm1.OnQuerySSLPort(APort: TIdPort; var AUseSSL: Boolean);
begin
  APort := 8080;   //如果不加上这个，就只能走默认的 443。这是 D12 的新特性。在 D10.4 的时候，不需要这个，也能在 8080 上面工作。
  AUseSSL := True;
end;
~~~

### 使用 OpenSSL 3.x 库
使用 Taurus 控件支持 OpenSSL 3.x。控件下载地址：
https://github.com/JPeterMugaas/TaurusTLS/tree/main

需要发布以下文件：
1. libcrypto-3.dll
2. libssl-3.dll
3. mis.mysite.net.cert.pem
4. mis.mysite.net.key.pem

代码如下：
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

procedure TForm1.StartServer;
begin
  if not FServer.Active then
  begin
    FServer.Bindings.Clear;
    FServer.DefaultPort := StrToInt(EditPort.Text); //这里默认是 8080
    FServer.OnQuerySSLPort := OnQuerySSLPort;
    FServer.Active := True;
  end;
end;

procedure TForm1.TaurusTLSServerIOHandler1GetPassword(ASender: TObject;
  var VPassword: string; const AIsWrite: Boolean; var VOk: Boolean);
begin
  VPassword := ''; //如果打开这个证书需要密码
end;
~~~

**重要说明**
在设计期的属性面板里面，TaurusTLSServerIOHandler1 的证书文件属性，填入文件名，运行时会出错。必须用代码给这两个个属性赋值。
TaurusTLSServerIOHandler1 只需要私钥和证书，不需要 CA 文件；IdServerIOHandlerSSLOpenSSL1 需要 CA 文件（RootCertFile）。

## 使用附件方式上传文件
服务器端接口函数的实现：
~~~
unit TestAttachImpl;
 
interface
 
uses Soap.InvokeRegistry, System.Types, System.SysUtils, Soap.XSBuiltIns, TestAttachIntf;
 
type
 
  { TTestAttach }
  TTestAttach = class(TInvokableClass, ITestAttach)
  public
    procedure GetAttach(var FileName: string; out Attach: TSOAPAttachment); stdcall;
    procedure PutAttach(const FileName: string; Attach: TSOAPAttachment); stdcall;
  end;
 
implementation
 
 
{ TTestAttach }
 
procedure TTestAttach.GetAttach(var FileName: string; out Attach: TSOAPAttachment);
var
  Fn: string;
begin
  //下载文件给客户端
  Fn := 'F:\H264Output.mp4';
 
  Attach := TSOAPAttachment.Create;
  Attach.SetSourceFile(Fn);
  FileName := ExtractFileName(Fn);
end;
 
procedure TTestAttach.PutAttach(const FileName: string;
  Attach: TSOAPAttachment);
begin
  //客户端上传附件
  Attach.SaveToFile(ExtractFilePath(GetModuleName(0)) + FileName);
end;
 
initialization
{ Invokable classes must be registered }
   InvRegistry.RegisterInvokableClass(TTestAttach);
end.
~~~

TSoapAttachment 来自：unit Soap.InvokeRegistry;

客户端上传文件的方法：
~~~
procedure TForm2.Button1Click(Sender: TObject);
var
  Attach: TSOAPAttachment;
  Intf: ITestAttach;
  Path, Fn: string;
begin
  //下载文件
  Path := ExtractFilePath(Application.ExeName);
  Attach := TSOAPAttachment.Create;
  Intf := HTTPRIO1 as ITestAttach;
  try
    Intf.GetAttach(Fn, Attach);
    Attach.SaveToFile(Path + Fn);
  finally
    Intf := nil;
    Attach.Free;
  end;
 
end;
 
procedure TForm2.Button2Click(Sender: TObject);
var
  Fn: string;
  Attach: TSOAPAttachment;
  Intf: ITestAttach;
begin
  //上传文件
  if OpenDialog1.Execute then
  begin
    Fn := OpenDialog1.FileName;
 
    Attach := TSOAPAttachment.Create;
    Attach.SetSourceFile(Fn);
    Intf := HTTPRIO1 as ITestAttach;
    try
      Intf.PutAttach(ExtractFileName(Fn), Attach);
    finally
      Intf := nil;
      Attach.Free;
    end;
  end;
end;
~~~

## FireBird 自增字段

**架构描述**：
FireBird - FireDAC - DataSetProvider - SOAP - ClientDataSet；

### FireBird 创建自增字段
一个 FireBird 的表，某个整数字段，设计为自增字段，实际上是有：
1. 一个“生成器”，Generator；负责每次调用输出一个顺序加 1 的数字；
2. 一个“触发器”，Trigger；负责每次新插入记录时，把这个字段的值写入从生成器获得的数字；

### FireDAC 的自增字段
使用 FireDAC 的 FdQuery 控件，对应上述 FireBird 的表，设计期，创建固定字段。选中自增字段，在属性面板里面，找到**AutoGenerateValue**属性，选中其值为：**arAutoInc**

### DataSetProvider 的设置
在对应上述 FdQuery 的 DataSetProvider 的属性面板里面设置：
1. ResolveToDataSet : 勾选;
2. Options 下拉开，勾选 poPropogateChanges;
3. 事件：AfterUpdateRecord 里面写代码，把 FdQuery 提交给数据库以后，数据库自增的字段值回传给客户端：
~~~
procedure TMyParamSoap.DataSetProvider1AfterUpdateRecord(Sender: TObject;
  SourceDS: TDataSet; DeltaDS: TCustomClientDataSet; UpdateKind: TUpdateKind);
begin
  DeltaDS.FieldByName('My_No').NewValue := SourceDS.FieldByName('My_No').AsInteger;
end;
~~~

### ClientDataSet 的操作
因为是数据库自增字段，多半是一个不能重复，必须有值的字段（比如可能是主键）；因此客户端插入新数据时，必须给一个值。这里可以插入连续的负数值。
因此，在它的 BeforeInsert 事件里面写：
~~~
  DataSet.Tag := DataSet.Tag -1;
  DataSet.FieldByName('My_No').AsInteger := DataSet.Tag;
~~~

**实际效果**
1. ClientDataSet 每次插入新记录，出现连续的 -1,-2,-3 的记录，避免了键值重复的异常错误；
2. ClientDataSet.ApplyUpdates 后，这些 -1,-2,-3 的记录，会自动变成数据库真实的自增的数值。