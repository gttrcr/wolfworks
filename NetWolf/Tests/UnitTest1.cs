using NetWolf;

namespace Tests
{
    [TestClass]
    public class UnitTest1
    {
        [TestMethod]
        public void TestMethod1()
        {
            WolframLink link = new();
            Transferable input = new Input(link, "2+2");
            Result res = link.Execute(input);
            Assert.IsTrue(res.Text == "4");
        }
    }
}