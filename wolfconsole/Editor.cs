using System.Text;

namespace Editor
{
    public static class Utils
    {
        public static StringBuilder ReplaceSubstring(this StringBuilder stringBuilder, int index, string replacement)
        {
            if (index + replacement.Length > stringBuilder.Length)
                stringBuilder.Append(new string(' ', replacement.Length));

            for (int i = 0; i < replacement.Length; ++i)
                stringBuilder[index + i] = replacement[i];

            return stringBuilder;
        }

        public static StringBuilder ReplaceSubstring(this StringBuilder stringBuilder, int index, char replacement)
        {
            return stringBuilder.ReplaceSubstring(index, replacement.ToString());
        }

        public static List<StringBuilder> ArbitraryAppend(this List<StringBuilder> list, int index)
        {
            for (int i = list.Count; i <= index; i++)
                list.Add(new StringBuilder());

            return list;
        }
    }

    class ConsoleEditor : TextWriter
    {
        private TextWriter _original;
        private List<StringBuilder> _lines;
        private ValueTuple<Int32, Int32> _pos;
        private ConsoleKeyInfo _keyInfo;
        private List<ConsoleKey> _actionKeys = new List<ConsoleKey> { ConsoleKey.Enter, ConsoleKey.LeftArrow, ConsoleKey.RightArrow, ConsoleKey.UpArrow, ConsoleKey.DownArrow, ConsoleKey.Backspace };
        private List<ConsoleKey> _handlerKeys = new List<ConsoleKey> { ConsoleKey.NoName, ConsoleKey.F1, ConsoleKey.F2, ConsoleKey.F3, ConsoleKey.F4, ConsoleKey.F5 };

        public delegate void Notify(ConsoleEditor instance, List<StringBuilder> lines);

        public event Notify HandlerF5 = default!;

        public override Encoding Encoding
        {
            get { return Encoding.Default; }
        }

        public ConsoleEditor()
        {
            this._original = Console.Out;
            _lines = new List<StringBuilder>();
            _lines.Add(new StringBuilder(""));
        }

        public void Write(string value, bool append = true)
        {
            if (append)
                _lines.ArbitraryAppend(_pos.Item2)[_pos.Item2].ReplaceSubstring(_pos.Item1, value);

            _original.Write(value);
        }

        public void Write(char value, bool append = true)
        {
            if (append)
                _lines.ArbitraryAppend(_pos.Item2)[_pos.Item2].ReplaceSubstring(_pos.Item1, value);

            _original.Write(value);
        }

        public void Edit()
        {
            Console.Clear();
            Console.SetCursorPosition(0, 0);
            Console.Title = "*";
            Console.SetOut(this);
            while (true)
            {
                _pos = Console.GetCursorPosition();
                _keyInfo = Console.ReadKey();
                if (_keyInfo.Modifiers.HasFlag(ConsoleModifiers.Control) && _keyInfo.Key == ConsoleKey.S)
                    break;

                if (!_actionKeys.Contains(_keyInfo.Key) && !_handlerKeys.Contains(_keyInfo.Key))
                    Write(_keyInfo.KeyChar);

                if (_keyInfo.Key == _actionKeys[0])
                {
                    _pos.Item1 = 0;
                    Console.SetCursorPosition(_pos.Item1, ++_pos.Item2);
                    _lines.Add(new StringBuilder(""));
                }
                else if (_keyInfo.Key == _actionKeys[1])
                    Console.SetCursorPosition(--_pos.Item1, _pos.Item2);
                else if (_keyInfo.Key == _actionKeys[2])
                    Console.SetCursorPosition(++_pos.Item1, _pos.Item2);
                else if (_keyInfo.Key == _actionKeys[3])
                    Console.SetCursorPosition(_pos.Item1, --_pos.Item2);
                else if (_keyInfo.Key == _actionKeys[4])
                    Console.SetCursorPosition(_pos.Item1, ++_pos.Item2);
                else if (_keyInfo.Key == _actionKeys[5])
                {
                    _pos.Item1--;
                    Console.SetCursorPosition(_pos.Item1, _pos.Item2);
                    Write(' ');
                    Console.SetCursorPosition(_pos.Item1, _pos.Item2);
                }

                if (_keyInfo.Key == _handlerKeys[5])
                    HandlerF5?.Invoke(this, new List<StringBuilder>(){_lines[_pos.Item2]});
            }

            System.IO.File.WriteAllText("out.txt", string.Join(Environment.NewLine, _lines.Select(x => x.ToString())));
            Console.Title = "Saved";
        }
    }
}