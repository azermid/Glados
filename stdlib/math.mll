
int min(int a, int b)
{
    if (a < b) {
        return a;
    }
    return b;
}

int max(int a, int b)
{
    if (a > b) {
        return a;
    }
    return b;
}

int abs(int a)
{
    if (a < 0) {
        return -a;
    }
    return a;
}


float mod(float x, float y)
{
    return x - x / y * y;
}

float cos(float x) {
    
    float res = 1.0;
    res -= x*x/2.0;
    res += x*x*x*x/24.0;  
    res -= x*x*x*x*x*x/720.0;
    res += x*x*x*x*x*x*x*x/40320.0;
    res -= x*x*x*x*x*x*x*x*x*x/3628800.0;
    return res;
}

float sin(float x) {
    float res = x;
    res -= x*x*x/6.0;
    res += x*x*x*x*x/120.0;
    res -= x*x*x*x*x*x*x/5040.0;
    res += x*x*x*x*x*x*x*x*x/362880.0;
    return res;
}

float exp(float x) {
    float res = 1.0;
    res += x;
    res += x*x/2.0;
    res += x*x*x/6.0;
    res += x*x*x*x/24.0;
    res += x*x*x*x*x/120.0;
    res += x*x*x*x*x*x/720.0;
    return res;
}