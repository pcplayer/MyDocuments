# UDP 命令包结构体的处理

使用 UDP 进行通讯，命令包通常定义为 Record；如果采用 Indy 控件，则网络数据是 TIdBytes 格式的 Byte 数组。
以前我的做法是把 TIdBytes 的内容搬移到 Record 结构体内。这样是做了一次数据搬移，可能需要消耗 CPU，消耗内存。

## 采用指针的方式减少 CPU 和内存的消耗

以下是测试成功的代码。

结构体的定义是：
~~~
TSmallVariant = record
    case Integer of
      0: (A: Byte);
      1: (B: Word);
  end;
~~~

### 测试代码之一：直接把结构体指针指向 TIdBytes：
~~~
procedure TForm1.Button3Click(Sender: TObject);
var
  R: TSmallVariant;
  R2: ^TSmallVariant;
  A: TIdBytes;
begin
  //结构体指针直接指向 TIdBytes 就能够解析结构体；
  //更进一步，如果我要根据命令头来判断该用哪个结构体，则可以把命令头这个整数都采用指针的方式转换为整数。
  //因此可以避免数据搬移；
  //如果采用变体结构体来做命令包，则所有命令包不管大小，最终的结构体都一样大，带来网络通讯的冗余。
  //所以还是不同命令采用不同结构体，降低网络冗余。
  R.A := 123;
  SetLength(A, SizeOf(R));

  Move(R, A[0], SizeOf(R));

  R2 := @(A[0]);

  Log(R2.A.ToString);   // Indy 控件收到的数据，可以直接通过指针的方式，把 Record 指过去，避免了数据搬移。
end;
~~~

上述代码中，直接定义 R2 为结构体指针类型的变量，把 R2 指向 TIdBytes 变量的第一个字节的地址，然后采用结构体的方式读 R2 的内容，成功。

### 测试代码之二：使用整数指针指向 TIdBytes：
如果我要根据命令头来判断该用哪个结构体，则可以把命令头这个整数都采用指针的方式转换为整数。因此，假设我的结构体的头两个字节是整数定义的命令头，也不需要把头两位字节搬移到一个整数变量里面去，而是直接指针，代码如下：
~~~
procedure TForm1.Button3Click(Sender: TObject);
var
  R: TSmallVariant;
  R2: ^TSmallVariant;
  A: TIdBytes;
begin
  //结构体指针直接指向 TIdBytes 就能够解析结构体；
  
  R.A := 123;
  SetLength(A, SizeOf(R));

  Move(R, A[0], SizeOf(R));

  R2 := @(A[0]);

  Log(R2.A.ToString);   // Indy 控件收到的数据，可以直接通过指针的方式，把 Record 指过去，避免了数据搬移。
end;
~~~

## 命令包结构体采用变体结构体还是普通结构体
1. 好处：所有命令，一个结构体定义搞定；
2. 坏处：不同命令的数据大小不同，如果采用一个变体结构体搞定，则数据少的命令包，也要占用更多数据，导致网络上传输的数据的冗余。

 因为如果采用变体结构体来做命令包，则所有命令包不管大小，最终的结构体都一样大，带来网络通讯的冗余。
 所以还是不同命令采用不同结构体，降低网络冗余。

### 良好的设计
1. 命令头是一个整数（byte 或者 word 或者 Integer），看命令有多少个。常规来看，如果命令采用分层结构，则第一个命令头，一个 Byte 足够。
   1.1. 命令最好采用分层结构。否则一个命令假设有几百个，则一个命令的 case 就有几百个，这样平板铺开，代码实在太复杂；  
2. case 命令头以后，根据不同的命令，把数据包分派给不同的命令处理方法；
3. 在每个不同的命令处理方法里面，对数据包进行不同的结构体转换处理。结构体转换直接采用本文上述的指针转换。转换之前判断一下数据包长途和结构体大小是否一致，避免溢出错误。
4. 命令分层，太多的复杂命令组织成一个多层次的树结构，每一层的处理，都可以按照上述根据命令头分派到不同的函数来处理，甚至可以每一层是一个类，分派到不同的类去处理。
  4.1. 多层命令的分派处理，以前是去掉前面的头，把后面的数据 Move 到下一层命令对应的 Record 里面。现在看起来只要记住偏移量，下一层命令对应的 Record 只需要指针指向偏移量的地址就可以了。
5. 发送的时候，因为 TIdUDP 是一次发送一个 TIdBytes 包，则必须把命令数据都拼装进一个包才行。假设命令数据是 Record，可以考虑直接使用指针；假设是多个 Record，则只能搬移数据。

## TIdBytes 和 TBytes
如果使用 Indy 作为通讯控件，则需要 TIdBytes；但更多的时候，我们可能使用 TBytes；
因为 TIdBytes 和 TBytes 在底层的数据结构，内存分布，以及生命周期管理的方式完全相同，因此，可以采用直接类型转换的方式，进行数据类型的转换赋值。
**生命周期**：动态数组的生命周期，都是引用计数。因此，我们不需要特意去释放它。

采用 RTTI 的方式查看这两个类型的内部定义如下：
~~~
 -- TBytes  --
Name: TArray<System.Byte>
Kind: tkDynArray
ElementType.Name: Byte
ElementType.TypeSize: 1
----
-- TIdBytes --
Name: TIdBytes
Kind: tkDynArray
ElementType.Name: Byte
ElementType.TypeSize: 1
~~~

获得上述内容的代码如下：

~~~
procedure TForm1.ShowDynArrayType(const ATypeInfo: PTypeInfo);
var
  ctx: TRttiContext;
  rType: TRttiType;
  dyn: TRttiDynamicArrayType;
begin
  rType := ctx.GetType(ATypeInfo);
  Log('Name: ' + rType.Name);
  Log('Kind: ' + GetEnumName(TypeInfo(TTypeKind), Ord(rType.TypeKind)));
  if rType is TRttiDynamicArrayType then
  begin
    dyn := TRttiDynamicArrayType(rType);
    Log('ElementType.Name: ' + dyn.ElementType.Name);
    Log('ElementType.TypeSize: ' + dyn.ElementType.TypeSize.ToString);
  end
  else
    Log('Not a dynamic array type according to RTTI.');
  Log('----');
end;
~~~

调用上述函数：
~~~
  Log('-- TBytes --');
  ShowDynArrayType(TypeInfo(TBytes));

  Log('-- TIdBytes --');
  ShowDynArrayType(TypeInfo(TIdBytes));
~~~

# 结论
1. 结构体和 TIdBytes 之间，可以直接使用指针方式，避免复制数据；
2. TIdBytes 和 TByte 之间，可以直接类型转换的方式赋值，避免复制数据。