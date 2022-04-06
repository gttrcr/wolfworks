using System;
using System.Collections.Generic;
using System.Linq;
using System.Text.RegularExpressions;
using System.Threading;
using Wolfram.NETLink;

namespace Wolf
{
    public class WolframLink
    {
        private readonly Mutex wolfMutex;
        private readonly MathKernel mathKernel;

        public WolframLink()
        {
            wolfMutex = new Mutex();
            mathKernel = new MathKernel()
            {
                ResultFormat = MathKernel.ResultFormatType.InputForm
            };

            //activate the kernel
            mathKernel.Compute();
        }

        //The only function that contains Compute function
        public string Wolf(string input)
        {
            wolfMutex.WaitOne();
            mathKernel.Compute(input);
            string obj = mathKernel.Result.ToString().Replace(" ", "").Replace("\r", "");
            wolfMutex.ReleaseMutex();

            return obj;
        }

        public List<string> Wolf(List<string> input)
        {
            return input.Select(x => Wolf(x)).ToList();
        }

        public string WolfSimplify(string input)
        {
            return Wolf("Simplify[" + input.Replace(" ", "") + "]");
        }

        public List<string> WolfSimplify(List<string> input)
        {
            return input.Select(x => WolfSimplify(x)).ToList();
        }

        public string SumOf(List<string> list)
        {
            string expr = "";
            int prog = 5;
            for (int i = 0; i < list.Count; i += prog)
            {
                int max = prog;
                if (i + prog > list.Count)
                    max = list.Count - i;

                expr += "(" + string.Join(")+(", list.GetRange(i, max)) + ")";
                expr = WolfSimplify(expr) + "+";
            }
            expr = expr.Substring(0, expr.Length - 1);
            return expr;
        }

        public List<string> RecursiveSimplify(string input)
        {
            string pattern = @"Abs\[([^\[\]A]+|(?<Level>\[)|(?<-Level>\]))+(?(Level)(?!))\]";
            MatchCollection matchList = Regex.Matches(input, pattern);
            List<string> list = matchList.Cast<Match>().Select(match => match.Value).Distinct().ToList();
            List<string> binary = Enumerable.Range(0, (int)Math.Pow(2, list.Count)).Select(x => ToBinary(x, list.Count)).ToList();

            List<string> output = Wolf(ReplaceAbs(input, list, binary));
            for (int i = 0; i < output.Count; i++)
            {
                if (output[i].Contains("Abs["))
                {
                    string tmp = output[i];
                    output.RemoveAt(i);
                    output.AddRange(RecursiveSimplify(tmp));
                    i = -1;
                }
            }

            return output;
        }

        public WolfType GetType(string str)
        {
            if (Wolf("NumberQ[" + str + "]").True())
                return WolfType.Number;

            if (Wolf("ArrayQ[" + str + "]").True())
                return WolfType.Array;

            if (Wolf("MatrixQ[" + str + "]").True())
                return WolfType.Matrix;

            if (Wolf("PolynomialQ[" + str + "]").True())
                return WolfType.Polynomial;

            return WolfType.Unknow;
        }

        public List<string> ToArray(string str)
        {
            List<string> ret = new List<string>();
            int Y = Convert.ToInt32(Wolf("Length[" + str + "]"));
            for (int y = 0; y < Y; y++)
                ret.Add(Wolf(str + "[[" + (y + 1) + "]]"));

            return ret;
        }

        public List<List<string>> ToMatrix(string str)
        {
            List<List<string>> ret = new List<List<string>>();
            List<string> arr = ToArray(str);
            for (int i = 0; i < arr.Count; i++)
                ret.Add(ToArray(arr[i]));

            return ret;
        }

        public void Dispose()
        {
            mathKernel.Dispose();
        }

        private List<string> ReplaceAbs(string input, List<string> abs, List<string> positive)
        {
            List<string> ret = new List<string>();
            List<string> args = abs.Select(x => x.Substring(4, x.Length - 5)).ToList();
            for (int i = 0; i < positive.Count; i++)
            {
                string retEl = input;
                string pos = positive[i];
                for (int p = 0; p < pos.Length; p++)
                {
                    string argsSign = "";
                    if (pos[p] == '1')
                        argsSign = args[p];
                    else if (args[p][0] == 's')
                        argsSign = "-" + string.Concat(args[p].Select(c => c == '-' ? '+' : c == '+' ? '-' : c));
                    else
                        argsSign = string.Concat(args[p].Select(c => c == '-' ? '+' : c == '+' ? '-' : c));

                    retEl = retEl.Replace(abs[p], "(" + argsSign + ")");
                }

                ret.Add(retEl);
            }

            return ret;
        }

        private string ToBinary(int x, int size = 32)
        {
            char[] buff = new char[size];

            for (int i = size - 1; i >= 0; i--)
            {
                int mask = 1 << i;
                buff[size - 1 - i] = (x & mask) != 0 ? '1' : '0';
            }

            return new string(buff);
        }
    }
}