import java.util.concurrent.atomic.AtomicIntegerArray;

class GetNSet implements State {
    private AtomicIntegerArray value;
    private byte maxval;

    private void copy_v(byte[] v)
    {
        int v_size = v.length;
        value = new AtomicIntegerArray(v_size);
        for (int i = 0; i != v_size; i++)
        {
            value.set(i, v[i]);
        }
    }

    GetNSet(byte[] v)
    {
        copy_v(v);
        maxval = 127;
    }
    GetNSet(byte[] v, byte m)
    {
        copy_v(v);
        maxval = m;
    }

    //methods:

    public int size() {return value.length();}

    public byte[] current() 
    { 
        byte[] temp = new byte[size()];
        for (int i = 0; i!= size(); i++)
        {
            temp[i] = (byte)value.get(i);
        }
        return temp;
    }

    public boolean swap(int i, int j) 
    {
        int temp_i = value.get(i);
        int temp_j = value.get(j);
        if (temp_i <= 0 || temp_j >= maxval)
        {
            return false;
        }
        value.set(i, temp_i - 1);
        value.set(j, temp_j + 1);
        return true;
    }

}
