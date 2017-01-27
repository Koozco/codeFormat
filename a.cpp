#include<iostream>
using namespace std;

int samogloski(string s){ int suma=0;
    for(int i=0; i<s.size(); i++)
    {  if(s[i]=='a' || s[i]=='e' || s[i]=='i' || s[i]=='o' || s[i]=='u' || s[i]=='y') suma++; }
    return suma;
}

int kody(string s)
{  int suma=0;   for(int i=0; i<s.size(); i++)   { suma+=int(s[i]); }  return suma;
}

main(){
    string s1, s2;
    getline(cin, s1);
    getline(cin, s2);
    cout<<kody(s1)<<" "<<samogloski(s1)<<endl;}
