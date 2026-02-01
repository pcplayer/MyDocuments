# TBytes 和 Record 之间的转换

基于命令包方式的通讯，通常把命令包定义为 Record；如果采用 Indy 控件，则真正通讯的数据包是 TIdBytes；

TIdBytes 和 TBytes 可以强制类型转换，它们底层的内存布局和引用计数，是相同的。因此，这里讨论 TBytes 和 Record 如何互相转换。

## 接收命令，从 TBytes 转换为 Record

1. 定义一个 Record 类型的指针变量，比如 var MyCmd: ^TMyRecord;
2. 把 TBytes 数据的指针赋予上述指针变量：MyCmd := @(AData[0];
3. 可以使用 MyCmd.MyField 对这个 Record 的字段进行访问了。

这里直接通过指针进行数据类型转换，避免了数据拷贝。

## 发送命令，从 Record 转换为 TBytes

发送数据的时候，一开始的业务逻辑的数据，通过定义的命令包结构体 TMyRecord 来进行赋值，最终把 TMyRecord 转换为 TBytes 用于网络发送。
这里必须把 TMyRecord 的数据拷贝到 TBytes 里面去。

~~~
 var
  MyRecord: TMyRecord;
  AData: TBytes;
begin
  SetLength(AData, SizeOf(MyRecord));
  Move(MyRecord, AData[0], SizeOf(MyRecord));
end;
~~~

为了简化代码，不用每个命令都写 SetLength 和 Move，可以把这个方法做成一个通用的方法：
~~~
procedure TForm1.MoveRecordToBytes(const Buffer; var ABytes: TBytes; const ASize: Integer);
begin
  SetLength(ABytes, ASize); //SizeOf(Buffer) 是 0，这里必须增加 ASize 这个参数；
  Move(Buffer, ABytes[0], ASize);
end;
~~~
上述方法的参数 Buffer 没有规定参数的类型。这样可以适应各种 Record， 都可以调用这个方法。

## 给结构体赋值
结构体的字段，如果需要字符串的值，不能是字符串或者动态数组比如 array of Char，而必须是 array[0..10] of Char  或者 array[0..10] of AnsiChar; 
如果使用动态数组，则无法直接把结构体本身拷贝数据给 TBytes。因为对于动态数组或者字符串，结构体本身的数据仅仅是指针，而不是真正的数据。

因此，业务代码中字符串的值，最终到代表命令包的 Record 时，需要从字符串转换为静态数组；转换函数是 StrLCopy；代码如下：
~~~
procedure TForm1.TestMoveRtoB;
var
  S1: AnsiString;
  S2: String;
  R: TTestRec;
  R2: ^TTestRec;
  B: TBytes;
begin
  S1 := 'pcplayer';
  S2 := '大家好嘛';

  //给结构体赋值，array [0..9] of AnsiChar
  StrLCopy(PAnsiChar(@(R.A)), PAnsiChar(S1), Length(R.A) -1);

  //给结构体赋值 array[0..9] of Char
  StrLCopy(PWideChar(@(R.B)), PWideChar(S2), Length(R.B) -1);

  //结构体的值必须拷贝数据到 TBytes
  MoveRecordToBytes(R, B, SizeOf(R));

  //TBytes 的值，可以通过指针的方式转换为结构体指针类型
  R2 := @(B[0]);

  Log(R2.A);
  Log(R2.B);
end;
~~~