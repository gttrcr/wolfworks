﻿using System.Net;
using System.Net.Sockets;
using System.Text;

namespace NetWolf
{
    public class StateObject
    {
        public Socket workSocket = null;
        public const int BufferSize = 1024;
        public byte[] buffer = new byte[BufferSize];
        public StringBuilder sb = new StringBuilder();
    }

    public class WolframLink
    {
        private readonly Mutex wolfMutex;
        private readonly Socket mathKernel;
        public List<Input> DefinedFunctions { get; private set; }

        public WolframLink(string ip)
        {
            wolfMutex = new Mutex();
            DefinedFunctions = new List<Input>();

            IPHostEntry ipHostInfo = Dns.GetHostEntry(Dns.GetHostName());
            IPAddress ipAddress = IPAddress.Parse(ip); //ipHostInfo.AddressList[1];
            const int port = 5000;
            IPEndPoint remoteEP = new IPEndPoint(ipAddress, port);
            mathKernel = new Socket(ipAddress.AddressFamily, SocketType.Stream, ProtocolType.Tcp);
            mathKernel.Connect(remoteEP);
        }

        private static void Receive(Socket client)
        {
            try
            {
                StateObject state = new StateObject();
                state.workSocket = client;
                client.BeginReceive(state.buffer, 0, StateObject.BufferSize, 0, new AsyncCallback(ReceiveCallback), state);
                receiveDone.WaitOne();
            }
            catch (Exception e)
            {
                Console.WriteLine(e.ToString());
            }
        }

        private static ManualResetEvent receiveDone = new ManualResetEvent(false);
        private static String response = String.Empty;
        private static void ReceiveCallback(IAsyncResult ar)
        {
            try
            {
                StateObject state = (StateObject)ar.AsyncState;
                Socket client = state.workSocket;
                int bytesRead = client.EndReceive(ar);

                if (bytesRead > 0)
                {
                    state.sb.Append(Encoding.ASCII.GetString(state.buffer, 0, bytesRead));
                    response = state.sb.ToString();
                    Console.WriteLine("Received {0} bytes to server.", bytesRead);
                    receiveDone.Set();
                }
                else
                    throw new InvalidDataException("0 byte received");
            }
            catch (Exception e)
            {
                Console.WriteLine(e.ToString());
            }
        }

        private static void Send(Socket client, String data)
        {
            byte[] byteData = Encoding.ASCII.GetBytes(data);
            //client.BeginSend(byteData, 0, byteData.Length, 0, new AsyncCallback(SendCallback), client);
            if (client.Send(byteData) != byteData.Length)
                throw new Exception();
            byte[] rec = new byte[1000];
            int length = client.Receive(rec);
            response = Encoding.ASCII.GetString(rec, 0, length);
            //sendDone.WaitOne();
        }

        //private static ManualResetEvent sendDone = new ManualResetEvent(false);
        //private static void SendCallback(IAsyncResult ar)
        //{
        //    try
        //    {
        //        Socket client = (Socket)ar.AsyncState;
        //        int bytesSent = client.EndSend(ar);
        //        Console.WriteLine("Sent {0} bytes to server.", bytesSent);
        //        sendDone.Set();
        //    }
        //    catch (Exception e)
        //    {
        //        Console.WriteLine(e.ToString());
        //    }
        //}

        //The only function that contains Compute function
        public Result Execute(Transferable input)
        {
            wolfMutex.WaitOne();
            
            //mathKernel.Compute(input.Text);
            //string obj = mathKernel.Result.ToString().Replace(" ", "").Replace("\r", "");

            Send(mathKernel, input.Text);
            //Receive(mathKernel);

            wolfMutex.ReleaseMutex();

            return new Result(this, response);
        }

        public Result Execute(string input)
        {
            return Execute(new Input(this, input));
        }

        public List<Result> Execute(List<Transferable> input)
        {
            return input.Select(x => Execute(x)).ToList();
        }

        public List<Result> Execute(List<string> input)
        {
            return input.Select(x => Execute(new Input(this, x))).ToList();
        }

        public void SetFunction(Transferable input)
        {
            if (!DefinedFunctions.Contains(input))
                DefinedFunctions.Add((Input)input);
            Execute(input);
        }

        public void SetFunction(string input)
        {
            SetFunction(new Input(this, input));
        }

        public Result CallFunction(Transferable input, List<Transferable> arguments, string name = "")
        {
            string inputStr = (name == "" ? name : name + "=") + input.Text + "[" + string.Join(",", arguments.Select(x => x.Text)) + "]";
            return Execute(inputStr);
        }

        public Result CallFunction(string input, List<string> arguments, string name = "")
        {
            return CallFunction(new Input(this, input), arguments.Select(x => new Input(this, x)).Cast<Transferable>().ToList(), name);
        }

        public Result Simplify(Transferable input = null, string name = "")
        {
            if (input == null)
                input = new Input(this, "%");
            string inputStr = (name == "" ? name : name + "=") + "Simplify[" + input.Text + "]";
            return Execute(inputStr);
        }

        public Result Length(Transferable input = null, string name = "")
        {
            if (input == null)
                input = new Input(this, "%");
            string inputStr = (name == "" ? name : name + "=") + "Length[" + input.Text + "]";
            return Execute(inputStr);
        }

        public Result Part(Transferable input = null, int index = 1, string name = "")
        {
            if (input == null)
                input = new Input(this, "%");
            string inputStr = (name == "" ? name : name + "=") + input.Text + "[[" + index + "]]";
            return Execute(inputStr);
        }

        public Result NumberQ(Transferable input = null, string name = "")
        {
            if (input == null)
                input = new Input(this, "%");
            string inputStr = (name == "" ? name : name + "=") + "NumberQ[" + input.Text + "]";
            return Execute(inputStr);
        }

        public Result ArrayQ(Transferable input = null, string name = "")
        {
            if (input == null)
                input = new Input(this, "%");
            string inputStr = (name == "" ? name : name + "=") + "ArrayQ[" + input.Text + "]";
            return Execute(inputStr);
        }

        public Result MatrixQ(Transferable input = null, string name = "")
        {
            if (input == null)
                input = new Input(this, "%");
            string inputStr = (name == "" ? name : name + "=") + "MatrixQ[" + input.Text + "]";
            return Execute(inputStr);
        }

        public Result PolynomialQ(Transferable input = null, string name = "")
        {
            if (input == null)
                input = new Input(this, "%");
            string inputStr = (name == "" ? name : name + "=") + "PolynomialQ[" + input.Text + "]";
            return Execute(inputStr);
        }

        public Result Abs(Transferable input = null, string name = "")
        {
            if (input == null)
                input = new Input(this, "%");
            string inputStr = (name == "" ? name : name + "=") + "Abs[" + input.Text + "]";
            return Execute(inputStr);
        }

        public Result Flatten(Transferable input = null, int index = 0, string name = "")
        {
            if (input == null)
                input = new Input(this, "%");
            string inputStr = (name == "" ? name : name + "=") + "Flatten[" + input + ", " + index + "]";
            return Execute(inputStr);
        }

        public Result Flatten(string input = "%", int index = 0, string name = "")
        {
            string inputStr = (name == "" ? name : name + "=") + "Flatten[" + input + ", " + index + "]";
            return Execute(inputStr);
        }

        public Result Export(string path, string obj = "%", string name = "")
        {
            string inputStr = (name == "" ? name : name + "=") + "Export[\"" + path + "\", " + obj + "]";
            return Execute(inputStr);
        }

        public Result Graph(string v, string e = "", string name = "", string options = "")
        {
            string inputStr = (name == "" ? name : name + "=") + "Graph[{" + v + "}" + (e == "" ? e : ", {" + e + "}") + (options == "" ? options : ", " + options) + "]";
            return Execute(inputStr);
        }

        //Add here driver methods

        public void Dispose()
        {
            mathKernel.Dispose();
        }

        #region DEPRECATED

        /*
        public List<string> RecursiveSimplify(string input)
        {
            string pattern = @"Abs\[([^\[\]A]+|(?<Level>\[)|(?<-Level>\]))+(?(Level)(?!))\]";
            MatchCollection matchList = Regex.Matches(input, pattern);
            List<string> list = matchList.Cast<Match>().Select(match => match.Value).Distinct().ToList();
            List<string> binary = Enumerable.Range(0, (int)Math.Pow(2, list.Count)).Select(x => ToBinary(x, list.Count)).ToList();

            List<string> output = Execute(ReplaceAbs(input, list, binary)).Select(x => x.Text).ToList();
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
        */

        #endregion DEPRECATED
    }
}