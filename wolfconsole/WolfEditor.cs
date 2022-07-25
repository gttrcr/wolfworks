using System.Text;
using Renci.SshNet;
using System.Diagnostics;
using System.Text.RegularExpressions;

namespace Editor
{
    class WolfEditor
    {
        private ConsoleEditor _ce;
        private SshClient? _ssh = default!;
        private ScpClient? _scp = default!;

        private string Interpreter(List<StringBuilder> obj, out bool transfer)
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

        private void HandlerF1(ConsoleEditor instance, object? obj)
        {
            instance.Write("------------------\n", false);
        }

        private void RemoteF5(ConsoleEditor instance, object? obj)
        {
            if (obj == null)
                return;

            instance.Write(" ... ", false);
            string code = Interpreter((List<StringBuilder>)obj, out bool transfer);
            string result = _ssh?.RunCommand("wolframscript -code '" + code + "'").Execute();
            if (transfer)
                using (Stream localFile = File.Create("./folder"))
                {
                    _scp?.Download("/home/pi/tmp.jpg", localFile);

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

        private void LocalF5(ConsoleEditor instance, object? obj)
        {
            if (obj == null)
                return;

            instance.Write(" ... ", false);
            string code = Interpreter((List<StringBuilder>)obj, out bool transfer);
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
        }

        public WolfEditor(bool remote, SshClient? ssh = null, ScpClient? scp = null)
        {
            _ssh = ssh;
            _scp = scp;
            _ce = new ConsoleEditor();
            _ce.HandlerF1 += HandlerF1;
            if (remote)
                _ce.HandlerF5 += RemoteF5;
            else
                _ce.HandlerF5 += LocalF5;
        }

        public void Edit(string? file = null)
        {
            _ce.Edit(file);
        }
    }
}