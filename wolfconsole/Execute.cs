using Renci.SshNet;
using System.Diagnostics;

namespace WMConsole
{
    class WMExec
    {
        public static void Execute(string path, bool remote, SshClient ssh, ScpClient scp)
        {
            Console.WriteLine("Executing ...");
            string code = Utils.Interpreter(path, out List<string> output_files);
            string result;
            if (remote)
            {
                result = ssh?.RunCommand("wolframscript -code '" + code + "'").Execute();
                for (int i = 0; i < output_files.Count; i++)
                {
                    path = "./" + Path.GetFileName(output_files[i]);
                    using (Stream localFile = File.Create(path))
                    {
                        scp?.Download(output_files[i], localFile);

                        ProcessStartInfo procStartInfo = new ProcessStartInfo("feh", path);
                        procStartInfo.RedirectStandardOutput = true;
                        procStartInfo.UseShellExecute = false;
                        procStartInfo.CreateNoWindow = true;
                        Process proc = new Process();
                        proc.StartInfo = procStartInfo;
                        proc.Start();
                    }
                }
            }
            else
            {
                ProcessStartInfo procStartInfo = new ProcessStartInfo("wolframscript", "-code " + code + "");
                procStartInfo.RedirectStandardOutput = true;
                procStartInfo.RedirectStandardError = true;
                procStartInfo.UseShellExecute = false;
                procStartInfo.CreateNoWindow = true;
                Process proc = new Process();
                proc.StartInfo = procStartInfo;
                proc.Start();
                result = proc.StandardOutput.ReadToEnd();
                proc.WaitForExit();
            }

            Console.Write(result);
        }
    }

    public static class Utils
    {
        public static string Interpreter(string path, out List<string> output_files)
        {
            string[] obj = File.ReadAllLines(path);
            output_files = new List<string>();
            for (int i = 0; i < obj.Length; i++)
            {
                if (obj[i].ToString().Contains("(*#*)"))
                {
                    string tmp_file = Path.GetFileName(Path.GetTempFileName()) + ".jpg";
                    output_files.Add(tmp_file);
                    obj[i] = obj[i].Replace("(*#*)", "");
                    obj[i] = "Export[\"" + tmp_file + "\"," + obj[i] + "]";
                }
            }

            return string.Join(Environment.NewLine, obj.Select(x => x.ToString()));
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
    }
}