``` ini

BenchmarkDotNet=v0.13.1, OS=macOS 13.2 (22D49) [Darwin 22.3.0]
Intel Core i5-7360U CPU 2.30GHz (Kaby Lake), 1 CPU, 4 logical and 2 physical cores
.NET SDK=7.0.102
  [Host]     : .NET 7.0.2 (7.0.222.60605), X64 RyuJIT DEBUG
  DefaultJob : .NET 7.0.2 (7.0.222.60605), X64 RyuJIT


```
|       Method |               Mean |            Error |           StdDev |
|------------- |-------------------:|-----------------:|-----------------:|
| moveTreeBTen |           214.6 ns |          1.32 ns |          1.03 ns |
|    moveTreeM |    17,355,022.6 ns |    192,606.98 ns |    160,835.55 ns |
| moveTree100M | 1,717,468,631.4 ns | 13,647,263.18 ns | 12,097,943.34 ns |
