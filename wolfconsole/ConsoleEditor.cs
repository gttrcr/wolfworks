using System.Text;
using System.Globalization;

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

        public static string Password()
        {
            var pass = string.Empty;
            ConsoleKey key;
            do
            {
                var keyInfo = Console.ReadKey(intercept: true);
                key = keyInfo.Key;

                if (key == ConsoleKey.Backspace && pass.Length > 0)
                {
                    Console.Write("\b \b");
                    pass = pass[0..^1];
                }
                else if (!char.IsControl(keyInfo.KeyChar))
                {
                    Console.Write("*");
                    pass += keyInfo.KeyChar;
                }
            } while (key != ConsoleKey.Enter);

            return pass;
        }

        public static bool IsPrintable(char c)
        {
            UnicodeCategory[] nonRenderingCategories = new UnicodeCategory[] { UnicodeCategory.Control, UnicodeCategory.OtherNotAssigned, UnicodeCategory.Surrogate };
            return Char.IsWhiteSpace(c) || !nonRenderingCategories.Contains(Char.GetUnicodeCategory(c));
        }
    }

    class ConsoleEditor : TextWriter
    {
        private TextWriter _original;
        private List<StringBuilder> _lines;
        private ValueTuple<Int32, Int32> _pos;
        private ConsoleKeyInfo _keyInfo;
        private bool _edited;

        public delegate void Notify(ConsoleEditor instance, object? obj);
        public event Notify HandlerF1 = default!;
        public event Notify HandlerF2 = default!;
        public event Notify HandlerF3 = default!;
        public event Notify HandlerF4 = default!;
        public event Notify HandlerF5 = default!;
        public event Notify HandlerF6 = default!;
        public event Notify HandlerF7 = default!;
        public event Notify HandlerF8 = default!;
        public event Notify HandlerF9 = default!;
        public event Notify HandlerF10 = default!;
        public event Notify HandlerF11 = default!;
        public event Notify HandlerF12 = default!;

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

        public void Write(string value, bool append = true, bool refresh = false)
        {
            if (append)
            {
                if (!refresh)
                    _lines.ArbitraryAppend(_pos.Item2)[_pos.Item2].ReplaceSubstring(_pos.Item1, value);
                else
                    _lines.ArbitraryAppend(_pos.Item2)[_pos.Item2] = new StringBuilder(value);
            }

            string refreshStr = "";
            if (refresh)
                refreshStr = new string(' ', Console.WindowWidth - value.Length);
            _original.Write(value + refreshStr);
        }

        public void Write(char value, bool append = true)
        {
            if (append)
                _lines.ArbitraryAppend(_pos.Item2)[_pos.Item2].ReplaceSubstring(_pos.Item1, value);

            _original.Write(value);
        }

        public void WriteLine(string value, bool append = true)
        {
            Write(value, append);
            Write(new string(' ', Console.WindowWidth - value.Length), false);
            Write(Environment.NewLine, false);
            _pos.Item1 = 0;
            _pos.Item2++;
        }

        public void RemoveLine(int lineNumber)
        {
            _lines.ArbitraryAppend(lineNumber).RemoveAt(lineNumber);
            for (int i = lineNumber; i < _lines.Count; i++)
            {
                Console.SetCursorPosition(0, i);
                Write(_lines[i].ToString(), false, true);
            }
            Write("", false, true);
            Console.SetCursorPosition(0, Math.Min(lineNumber, _lines.Count));
        }

        public void Edit(string? file = null)
        {
            Console.Clear();
            if (file == null || !File.Exists(file))
                Console.Title = "Untitled.wl";
            else if (File.Exists(file))
            {
                Console.Title = file;
                File.ReadAllLines(file).ToList().ForEach(x => WriteLine(x));
            }

            Console.SetCursorPosition(0, 0);
            Console.SetOut(this);
            while (true)
            {
                _pos = Console.GetCursorPosition();
                _keyInfo = Console.ReadKey();
                if (_keyInfo.Modifiers.HasFlag(ConsoleModifiers.Control))
                {
                    switch (_keyInfo.Key)
                    {
                        case ConsoleKey.S:
                            System.IO.File.WriteAllText(file, string.Join(Environment.NewLine, _lines.Select(x => x.ToString())));
                            Console.Title = file;
                            _edited = false;
                            break;
                    }
                }

                if (Utils.IsPrintable(_keyInfo.KeyChar))
                {
                    if (!_edited)
                    {
                        Console.Title = file + "*";
                        _edited = true;
                    }
                    Write(_keyInfo.KeyChar);
                }

                switch (_keyInfo.Key)
                {
                    case ConsoleKey.Enter:
                        _pos.Item1 = 0;
                        Console.SetCursorPosition(_pos.Item1, ++_pos.Item2);
                        _lines.Add(new StringBuilder(""));
                        break;
                    case ConsoleKey.LeftArrow:
                        Console.SetCursorPosition(--_pos.Item1, _pos.Item2);
                        break;
                    case ConsoleKey.RightArrow:
                        Console.SetCursorPosition(++_pos.Item1, _pos.Item2);
                        break;
                    case ConsoleKey.UpArrow:
                        Console.SetCursorPosition(_pos.Item1, --_pos.Item2);
                        break;
                    case ConsoleKey.DownArrow:
                        Console.SetCursorPosition(_pos.Item1, ++_pos.Item2);
                        break;
                    case ConsoleKey.Backspace:
                        _pos.Item1--;
                        Console.SetCursorPosition(_pos.Item1, _pos.Item2);
                        Write(' ');
                        Console.SetCursorPosition(_pos.Item1, _pos.Item2);
                        break;
                    case ConsoleKey.Delete:
                        if (_lines[_pos.Item2].Length > 0)
                            Write(_lines[_pos.Item2].ToString().Substring(1), true, true);
                        else
                            RemoveLine(_pos.Item2);
                        _pos.Item1 = 0;
                        Console.SetCursorPosition(_pos.Item1, _pos.Item2);
                        break;
                }

                switch (_keyInfo.Key)
                {
                    case ConsoleKey.F1:
                        HandlerF1?.Invoke(this, null);
                        break;
                    case ConsoleKey.F2:
                        HandlerF1?.Invoke(this, null);
                        break;
                    case ConsoleKey.F3:
                        HandlerF1?.Invoke(this, null);
                        break;
                    case ConsoleKey.F4:
                        HandlerF1?.Invoke(this, null);
                        break;
                    case ConsoleKey.F5:
                        HandlerF5?.Invoke(this, new List<StringBuilder>() { _lines[_pos.Item2] });
                        break;
                    case ConsoleKey.F6:
                        HandlerF1?.Invoke(this, null);
                        break;
                    case ConsoleKey.F7:
                        HandlerF1?.Invoke(this, null);
                        break;
                    case ConsoleKey.F8:
                        HandlerF1?.Invoke(this, null);
                        break;
                    case ConsoleKey.F9:
                        HandlerF1?.Invoke(this, null);
                        break;
                    case ConsoleKey.F10:
                        HandlerF1?.Invoke(this, null);
                        break;
                    case ConsoleKey.F11:
                        HandlerF1?.Invoke(this, null);
                        break;
                    case ConsoleKey.F12:
                        HandlerF1?.Invoke(this, null);
                        break;
                }
            }
        }
    }
}