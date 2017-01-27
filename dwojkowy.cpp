#include <iostream>
using namespace std;
int s[100000000];
int z[100000000];

int main () {
int l,p,a;
cin>>l;

for (int i=0;i<l;i++)
    {
     cin>>a;
     p=0;
     while (a>0)
     {if (a==1) {s[p]=1; break;}
       if (a%2==0) {s[p]=0; a=a/2;} else {s[p]=1; a=(a-1)/2;};
     p++;};
     while (p>-1)
     {cout<<s[p]; p--;};
     cout<<endl;
     }

