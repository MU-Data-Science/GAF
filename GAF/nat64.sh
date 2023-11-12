HOME=/home/ubuntu
  
wget --output-document=$HOME/temp.txt https://nat64.net/
egrep -o '([a-f0-9:]+:+)+[a-f0-9]+' $HOME/temp.txt > $HOME/ip.txt
sed -i -e 's/^/nameserver /' $HOME/ip.txt

cp /etc/resolv.conf /etc/resolv.conf_backup
line=$(grep -n 'nameserver' /etc/resolv.conf | cut -d ":" -f 1)
{ head -n $(($line-1)) /etc/resolv.conf; cat $HOME/ip.txt; tail -n +$line /etc/resolv.conf; } > /etc/resolv.conf_add
cp /etc/resolv.conf_add /etc/resolv.conf

rm -f $HOME/temp.txt
rm -f $HOME/ip.txt
rm /etc/resolv.conf_add
