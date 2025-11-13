# 再谈 FireBird 自增字段在ClientDataSet里如何处理

## 架构
**数据库**：FireBird;

**客户端**：ClientDataSet；

**中间层**（服务器端）：FireDAC 的 FdQuery；
**整体架构**：WebService / MIDAS

使用 ClientDataSet 就是使用了 Delphi 的 MIDAS 框架。在架构上，服务器端和客户端是分布在不同的电脑上，通过网络的方式进行交互。我比较喜欢采用 Delphi 自己的 WebService 的架构，服务器端是一个 WebService 服务器，客户端采用 HTTPRIO 或者 SoapConnection 连接服务器端。

使用这个框架的好处是，客户端和服务器端，在运行期，是可以不连接的。仅仅是从服务器读取数据或者向服务器端提交数据时，才需要连接。
因此，这种架构，非常适合一个硬件资源不大的服务器端，可以同时直接巨多的客户端。因为客户端并没有和服务器端保持一个长期连接。大部分时间，用户在客户端操作数据时，客户端和服务器端之间并没有交互，也就没有连接。

但是，因为没有连接，则客户端在使用数据库的自增字段时，就需要一点小技巧，但是并不需要写很多代码。

本文是针对标题描述的问题的一个测试代码进行撰写，这个测试代码是把作为服务器端的 FdQuery1 和 DataSetProvider1 放到 Form4 上面，同时，作为客户端的 ClientDataSet1 也放到 Form4 上面。在实际的应用程序中，ClientDataSet1 在另外一个单独的客户端程序里面。 

## 描述
首先， ClientDataSet 的字段里面，有一个 AutoGenerateValue 属性，设置为 arAutoInc 是没有任何作用的，不需要去在这个属性上面浪费时间。

### 前提：
在 FireBird 里面设计一个表，其中一个字段，设置为自增字段。对于 FireBird 数据库来说，它会产生一个 Generator （生成器）用来自动计算自增的那个整数，然后产生一个 Trigger （触发器）来在有新数据插入时，把 Generator 产生的数字写入自增字段。

### 操作：
以下描述都是设计期，无需写代码：

1. FireBird 设置一个整数字段为自增字段，也就是它会有一个生成器和一个触发器，当插入新记录时，触发器从生成器获得自增的数字写入新插入记录的该字段；
2. FireDAC 的 FDQuery1 对应有自增字段的表（select * from MyTable），设计期创建固定字段，然后选择该自增字段，在属性面板，找到该字段的属性：AutoGenerateValue，设置为 arAutoInc;
3. DataSetProvider1 指向 FdQuery1；设计期，属性面板里面找到它的 Options 拉开，找到属性：[poPropogateChanges]，勾选。这个属性用于将运行期的自增字段的值返回给 ClientDataSet1；
4. 属性面板里面，找到 DataSetProvider1 的属性 ResolveToDataSet 设置为 True；
5. ClientDataSet1 的 ProviderName 设置为 DataSetProvider1；

#### 接下来写一点代码

**客户端代码**：
~~~
procedure TDmInvoice.CldInvoice2024AfterInsert(DataSet: TDataSet);
begin
  with ClientDataSet1 do
  begin
    Tag := Tag -1;
    FieldByName('SNO').AsLargeInt := Tag;
  end;
  
end;
~~~
   这段代码的意思是，当 ClientDataSet1 有新记录插入，则自动为这个自增字段（一般是主键，必须要有值）写入一个顺序减少的负数。这里拿它的 Tag 属性来存储这个负整数。
 
**服务器端代码**：
选择 DataSetProvider1, 属性面板里面找到它的事件：AfterUpdateRecord ，双击，产生代码框架，填入以下代码：
~~~
procedure TForm4.DataSetProvider1AfterUpdateRecord(Sender: TObject;
  SourceDS: TDataSet; DeltaDS: TCustomClientDataSet; UpdateKind: TUpdateKind);
begin
  DeltaDS.FieldByName('SNO').NewValue := SourceDS.FieldByName('SNO').Value;
end;
~~~

#### 上述服务器端代码的解释：
**目的**：
客户端提交后，数据库把客户端提交的新插入记录，自动产生新的自增值；但我们要让这个值返回给客户端，让客户端把对应记录的负数值，变成数据库产生的自增值。
表现为客户端提交后，负数的自增字段的值自动变成了数据库里面的新记录的自增的整数值。

**原理**：
给 DataSetProvider1 的 Options 属性添加了 poPropogateChanges 则它会把客户端提交的 Delta 修改后返回给客户端。

## 总结
这里利用了 FireDAC 的字段的 AutoGenerateValue 属性设置为 arAutoInc 来对应 FireBird 的自增字段。

单靠 ClientDataSet 自己去提交是搞不定的。而且 ClientDataSet 的 AutoGenerateValue 属性设置为 arAutoInc 不起任何作用。

这恐怕是写最少代码的办法了。

如果客户端不是 3 层架构，而是直接使用 FireDAC 作为客户端，则一行代码都不用写，设置好字段的属性就搞定了。

### 最后总结
这里的整个过程是：
1. ClientDataSet1 在新插入记录时，客户端自己为自己创建主键的负数值，填充用；
2. ClientDataSet1 提交，是提交数据给 DataSetProvider1，此时，默认是 DataSetProvider1 自己产生 SQL 语句去将客户端提交的数据写入数据库。但是，因为这里给 DataSetProvider1 的 ResolveToDataSet 属性设置为 True，则它把数据丢给它的 DataSet 也就是 FdQuery1 去向数据库提交。
3. FdQuery1 在向数据库提交的时候，因为在设计期已经指定该字段的 AutoGenerateValue 为 arAutoInc，因此，FdQuery1 会正确地向 FireBird 提交新插入记录，并且从 FireBird 获得新产生的自增字段值。
4. 然后，在 DataSetProvider1AfterUpdateRecord 事件方法里，从 SourceDS（这里就是 FdQuery1）获得数据库产生的自增值，赋予客户端提交的 DeltaDS，然后 MIDAS 会自动把这个值返回给客户端的 ClientDataSet1。
5. 到此，整个过程圆满完成。