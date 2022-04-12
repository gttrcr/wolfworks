using Renci.SshNet;

namespace WMConsole
{
    class WMConsole
    {
        static List<Tuple<string, List<string>>> Args(string[] args)
        {
            List<Tuple<string, List<string>>> ret = new List<Tuple<string, List<string>>>();
            args = args.Select(x => x.Trim()).ToArray();
            for (int i = 0; i < args.Length;)
            {
                if (args[i].StartsWith("-") && args[i].Substring(1).Length > 0)
                {
                    string arg = args[i].Substring(1);
                    List<string> argArgs = new List<string>();
                    while (++i < args.Length && !args[i].StartsWith("-"))
                        argArgs.Add(args[i]);

                    ret.Add(new Tuple<string, List<string>>(arg, argArgs));
                }
            }

            return ret;
        }

        static void Main(string[] args)
        {
            List<Tuple<string, List<string>>> largs = Args(args);

            string kernel = "";
            if (largs.Select(x => x.Item1).Contains("k"))
            {
                if (largs.Find(x => x.Item1 == "k").Item2[0] == "local")
                    kernel = "1";
                else if (largs.Find(x => x.Item1 == "k").Item2[0] == "remote")
                    kernel = "2";
            }
            else
                do
                {
                    Console.WriteLine("Wolfram Kernel is\n1.  local\n2.  remote");
                    kernel = Console.ReadLine();
                }
                while (kernel != "1" && kernel != "2");

            string host = "";
            string user = "";
            string passwd = "";
            if (kernel == "2")
            {
                if (largs.Select(x => x.Item1).Contains("host"))
                    host = largs.Find(x => x.Item1 == "host").Item2[0];
                else
                {
                    Console.Write("Host: ");
                    host = Console.ReadLine();
                }

                if (largs.Select(x => x.Item1).Contains("u"))
                    user = largs.Find(x => x.Item1 == "u").Item2[0];
                else
                {
                    Console.Write("User: ");
                    user = Console.ReadLine();
                }

                if (largs.Select(x => x.Item1).Contains("p"))
                    passwd = largs.Find(x => x.Item1 == "p").Item2[0];
                else
                {
                    Console.Write("Password: ");
                    passwd = Utils.Password();
                }
            }

            string path = "";
            if (largs.Select(x => x.Item1).Contains("f"))
                path = largs.Find(x => x.Item1 == "f").Item2[0];
            else
            {
                Console.Write("File path: ");
                path = Console.ReadLine();
            }

            try
            {
                SshClient ssh = default!;
                ScpClient scp = default!;
                bool ok = true;
                if (kernel == "2")
                {
                    Console.WriteLine("Connecting ...");
                    ssh = new SshClient(host, 22, user, passwd);
                    ssh.Connect();
                    scp = new ScpClient(host, user, passwd);
                    scp.Connect();

                    ok = ssh.IsConnected && scp.IsConnected;
                }

                if (ok)
                    WMExec.Execute(path, kernel == "2", ssh, scp);
                else
                    Console.WriteLine("Something went wrong");
            }
            catch (Exception ex)
            {
                Console.WriteLine(ex.Message);
            }
        }
    }
}
