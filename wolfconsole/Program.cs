using Renci.SshNet;
using Editor;
using System.Text;
using System.Text.RegularExpressions;
using System.Diagnostics;

namespace WMConsole
{
    class WMConsole
    {
        private static SshClient _ssh = default!;
        private static ScpClient _scp = default!;

        static string Interpreter(List<StringBuilder> obj, out bool transfer)
        {
            string pattern = @"Plot\[(.*?)\]";
            Regex rgx = new Regex(pattern);
            transfer = false;

            for (int i = 0; i < obj.Count; i++)
            {
                MatchCollection mc = rgx.Matches(obj[i].ToString());
                if (mc.Count == 1)
                {
                    transfer = true;
                    obj[i] = new StringBuilder("graphicsObject=" + obj[i] + "; Export[\"tmp.jpg\", graphicsObject];");
                }
            }
            return string.Join(Environment.NewLine, obj.Select(x => x.ToString()));
        }

        static void RemoteF5(ConsoleEditor instance, List<StringBuilder> obj)
        {
            instance.Write(" ... ", false);
            string code = Interpreter(obj, out bool transfer);
            string result = _ssh.RunCommand("wolframscript -code '" + code + "'").Execute();
            if (transfer)
                using (Stream localFile = File.Create("./folder"))
                {
                    _scp.Download("/home/pi/tmp.jpg", localFile);

                    ProcessStartInfo procStartInfo = new ProcessStartInfo("feh", "./folder");
                    procStartInfo.RedirectStandardOutput = true;
                    procStartInfo.UseShellExecute = false;
                    procStartInfo.CreateNoWindow = true;
                    Process proc = new Process();
                    proc.StartInfo = procStartInfo;
                    proc.Start();
                }
            if (!result.Contains("Null"))
                instance.Write(result, false);
        }

        static void LocalF5(ConsoleEditor instance, List<StringBuilder> obj)
        {
            instance.Write(" ... ", false);
            string code = Interpreter(obj, out bool transfer);
            ProcessStartInfo procStartInfo = new ProcessStartInfo("wolframscript", "-code " + code + "");
            procStartInfo.RedirectStandardOutput = true;
            procStartInfo.RedirectStandardError = true;
            procStartInfo.UseShellExecute = false;
            procStartInfo.CreateNoWindow = true;
            Process proc = new Process();
            proc.StartInfo = procStartInfo;
            proc.Start();
            string result = proc.StandardOutput.ReadToEnd();
            proc.WaitForExit();
            if (!result.Contains("Null"))
                instance.Write(result, false);

            //string result = _ssh.RunCommand("wolframscript -code '" + code + "'").Execute();
            //if (transfer)
            //    using (Stream localFile = File.Create("./folder"))
            //    {
            //        _scp.Download("/home/pi/tmp.jpg", localFile);
            //
            //        System.Diagnostics.ProcessStartInfo procStartInfo = new System.Diagnostics.ProcessStartInfo("feh", "./folder");
            //        procStartInfo.RedirectStandardOutput = true;
            //        procStartInfo.UseShellExecute = false;
            //        procStartInfo.CreateNoWindow = true;
            //        System.Diagnostics.Process proc = new System.Diagnostics.Process();
            //        proc.StartInfo = procStartInfo;
            //        proc.Start();
            //    }
            //if (result != "Null\n")
            //    instance.Write(result, false);
        }

        static void Main(string[] args)
        {
            Console.Clear();
            Console.WriteLine(
@"Wolfram Kernel is
1.  local
2.  remote");

            try
            {
                bool ok = false;
                ConsoleEditor ce = new ConsoleEditor();
                string? input = Console.ReadLine();
                if (input == "1")
                {
                    ok = true;
                    ce.HandlerF5 += LocalF5;
                }
                else if (input == "2")
                {
                    Console.WriteLine("Remote connection is available only on SSH");
                    Console.Write("IP: ");
                    string? ip = Console.ReadLine();
                    Console.Write("User: ");
                    string? user = Console.ReadLine();
                    Console.Write("passwd: ");
                    string? passwd = Console.ReadLine();

                    _ssh = new SshClient(ip, 22, user, passwd);
                    _ssh.Connect();
                    _scp = new ScpClient(ip, user, passwd);
                    _scp.Connect();

                    ok = _ssh.IsConnected && _scp.IsConnected;
                    ce.HandlerF5 += RemoteF5;
                }

                if (ok)
                    ce.Edit();
                else
                    Console.WriteLine("Something wend wrong");
            }
            catch (Exception ex)
            {
                Console.WriteLine(ex.Message);
            }
        }
    }
}