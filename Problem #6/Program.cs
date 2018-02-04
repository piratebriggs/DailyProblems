using System;
using System.Collections.Generic;

namespace Problem__6
{
    public struct ListItem {
        public int Value {get;set;}
        public int BothIndex {get;set;}
    }
    public class XorList {
        private List<ListItem> _listStorage {get;set;} = new List<ListItem>();
        private int? _headIndex = null;

        public void Add(int value) {

            if(!_headIndex.HasValue) {
                _listStorage.Add(new ListItem(){Value=value, BothIndex=0});
                _headIndex = 1;
                return;
            }
            
            int tailIndex = FindTailIndex();
            var nextIndex = _listStorage.Count;
            _listStorage.Add(new ListItem(){Value=value, BothIndex=0 ^ tailIndex });
            _headIndex = nextIndex;
            
        }
        public int Get(int index) {
            int currentIndex = _headIndex.Value;
            int nextIndex = 0;
            int count = 0;
            while(count <=index) {
                var xorIndex = _listStorage[currentIndex-1].BothIndex;

                nextIndex = xorIndex  ^ currentIndex;
                if(nextIndex == 0) {
                    break;
                }

                currentIndex = nextIndex;
            } 
            return _listStorage[currentIndex-1].Value;
            
        }
        private int FindTailIndex() {
            int currentIndex = _headIndex.Value;
            int nextIndex = 0;
            while(true) {
                var xorIndex = _listStorage[currentIndex-1].BothIndex;

                nextIndex = xorIndex  ^ currentIndex;
                if(nextIndex == 0) {
                    break;
                }

                currentIndex = nextIndex;
            } 
            return currentIndex;
        }
    } 
    class Program
    {
        static void Main(string[] args)
        {
            var  cut =new XorList();
            cut.Add(1);
            cut.Add(2);
            cut.Add(3);

            cut.Get(2);

            Console.WriteLine("Hello World!");
        }
    }
}
