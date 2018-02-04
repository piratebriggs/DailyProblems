using System;
using System.Collections.Generic;
using System.Linq;

namespace Product
{
    class Program
    {
        static void Main(string[] args)
        {
            var input = new List<int>() {1,2,3,4,5};
            var intermediate = new Stack<int>();
            var output = new List<int>();
            int accumulator = 1;
            intermediate.Push(1); // make lists equal length
            foreach(var val in input.Take(input.Count-1))   // process all but the last one
            {
                accumulator =  accumulator*val;
                intermediate.Push(accumulator);
            }
            input.Reverse();
            accumulator = 1;
            foreach(var val in input)
            {
                output.Add(accumulator * intermediate.Pop());                
                accumulator *= val;
            }
            output.Reverse();
            Console.WriteLine("Result! {0}", String.Join(",",output));  // [120, 60, 40, 30, 24]. 
        }
    }
}
