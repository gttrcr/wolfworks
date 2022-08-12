﻿using System.Collections.Generic;
using System;

namespace NetWolf
{
    public abstract class Transferable
    {
        public WolframLink Link { get; private set; }
        public string Text { get; private set; }

        public Transferable(WolframLink link, string text = "")
        {
            Link = link;
            Text = text;
        }

        public bool IsBooleanTrue()
        {
            return Text == "True";
        }

        public bool IsBooleanFalse()
        {
            return Text == "False";
        }
    }

    public class Input : Transferable
    {
        public Input(WolframLink link, string text = "") : base(link, text)
        {

        }
    }

    public static class TransferableExtension
    {
        public static List<Result> ToArray(this Transferable res)
        {
            List<Result> ret = new List<Result>();
            int Y = Convert.ToInt32(res.Link.Length(new Input(res.Link, res.Text)).Text);
            for (int y = 0; y < Y; y++)
                ret.Add(res.Link.Part(new Input(res.Link, res.Text), y + 1));

            return ret;
        }

        public static List<List<Result>> ToMatrix(this Result res)
        {
            List<List<Result>> ret = new List<List<Result>>();
            List<Result> arr = ToArray(res);
            for (int i = 0; i < arr.Count; i++)
                ret.Add(ToArray(new Result(res.Link, arr[i].Text)));

            return ret;
        }
    }

    public class Result : Transferable
    {
        public Result(WolframLink link, string text = "") : base(link, text)
        {

        }

        public WolfType Type()
        {
            if (Link.NumberQ(this).IsBooleanTrue())
                return WolfType.Number;

            if (Link.ArrayQ(this).IsBooleanTrue())
                return WolfType.Array;

            if (Link.MatrixQ(this).IsBooleanTrue())
                return WolfType.Matrix;

            if (Link.PolynomialQ(this).IsBooleanTrue())
                return WolfType.Polynomial;

            return WolfType.Unknow;
        }
    }
}