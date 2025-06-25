# Delphi 的泛型和匿名函数：对 TObjectList<> 做排序

## 关于泛型的 TObjectList<T>

首先，需要 uses System.Generics.Collections, System.Generics.Defaults;
System.Generics.Defaults 用于 TComparer 

假设有一个自己定义的类：
~~~
  TMyObj = class
  private
  public
    FMyName: string;
  end;
~~~

然后给这个类来一个：
~~~
  FList: TObjectList<TMyObj>;
~~~
那么，对这个 FList 进行排序，就是在它的 Sort 方法里面，写入用于排序的匿名函数：
~~~
procedure TForm2.SortMyList;
begin
  FList.Sort(
    TComparer<TMyObj>.Construct(
      function (const L, R: TMyObj): Integer
      begin
        Result := CompareStr(L.FMyName, R.FMyName);
      end
    )
  );
end;
~~~
