# Delphi 的泛型容器如何管理内部对象的生命周期

## 概念
Delphi 的泛型容器  TDictionary<Key, Value> 里面的 Key 和 Value 可以是任何类型的数据。

比如：
~~~
  MyDict: TDictionary<Integer, string>;
~~~

又或者：
~~~
  MyDict: TDictionary<Integer, TPerson>
~~~

~~~
TPerson = class
 
  FMyName: string;
 
  FMyAge: Integer;
 
private
 
public
 
end;
~~~

# 问题
如果 Value 是一个对象，当容器内容被清理，或者容器本身被释放时，这个对象如何释放？

比如：
~~~
var
  APerson: TPerson;
 
begin
  APerson := TPerson.Create;
  APerson.FMyName := 'abcd';
  APerson.FMyAge := 25;
 
  MyDict.Add(APerson.FMyAge, APerson);
end;
~~~

# 解决方案之一
对于 TDicitonary 来说，它有一个事件：OnValueNotify

因此，对于上述代码，为它增加一个事件方法，代码如下：
~~~
var
  APerson: TPerson;
 
begin
  APerson := TPerson.Create;
  APerson.FMyName := 'abcd';
  APerson.FMyAge := 25;
 
  MyDict.Add(APerson.FMyAge, APerson);
  MyDict.OnValueNotify := Self.DoOnMaiHeaderListValueNotify;
end;
 
 
//------------
procedure TForm1.DoOnMaiHeaderListValueNotify(Sender: TObject;
  const Item: TPerson;
  Action: System.Generics.Collections.TCollectionNotification);
begin
  // TDictionary 当有 Item 加入或删除时触发这个事件方法。
  case Action of
    cnAdded: ;
 
    cnRemoved:
    begin
      Item.Free; //释放里面的对象。
    end;
 
    cnExtracted: ;
  end;
end;
~~~
上述代码中，我们增加了一个方法，把这个方法赋予 TDictionary 的 OnValueNotify 事件。

因此，当这个 TDictionary 内部的数据被移除时，会触发事件，调用到这个方法。我们在这个方法里面写代码：Item.Free 来释放对象。

# 解决方案之二
使用 TDictionary 稍微麻烦一点。有更简单的：TObjectDictionary；

看名字就知道，这个类和 TDictionary 功能类似，但它会自动管理内部的对象。
~~~
MyDict: TObjectDictionary<Integer, TPerson>;
 
MyDict := TObjectDictionary<string, TPerson>.Create([doOwnsValues]); //增加参数 doOwnsValues 就让它管理内部对象的生命周期。不需要再额外很其它的代码了。
~~~
这里的关键点就是它的构造函数的参数：

**Create([doOwnsValues]);**

# 结束
