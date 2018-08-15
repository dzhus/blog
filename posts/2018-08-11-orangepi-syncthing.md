---
lang: en
tags:
- hardware
---

# Orange Pi Zero as a Syncthing node

[Syncthing][] is pretty great, it just works, which is becoming rare
for software these days. Syncthing is peer-to-peer, which means that
there doesn't have to be a central node that can die, refuse service
or unexpectedly increase the price. You can replicate and share your
files across a network of nodes *you* control, which can include your
laptop, a storage server, an [Android][] phone etc., without relying
on proprietary software or a third party such as Dropbox. Anything can
work with files. Syncthing nodes don't even have to be on the
Internet. Cloud providers should provide Syncthing-as-a-service as a
nicer way to interface laptops and phones with commoditised blob
storage such as S3 offered by AWS and GCP.

Meanwhile, a cheap ARM-based computer such as [Orange Pi Zero][opi]
makes a good Syncthing node. This is the setup which works great for
me:

1. An Orange Pi Zero with network access (roughly $10). I simply use
   Wi-Fi. It's rather bad on this particular board with Armbian 5.30
   (Ubuntu Xenial-based, 3.4.x kernel, June 2017), wired Ethernet
   works much better. Quite enough for my Syncthing needs though.

2. For the actual storage, I used a 2.5-inch laptop HDD with a USB
   enclosure (roughly $18 for both). I simply had a few 140GB spares
   lying around (which inspired this work in the first place). Picking
   a bigger SD card for the Pi is another option of course, but
   they're still pricier than HDDs at this point. Good thing about USB
   connection is that you can grab the disk and go plug it elsewhere
   even without Syncthing. When using only an SD card you could grab
   the whole Pi box though :)

3. Orange Pi will struggle to pump enough power to the HDD. I used a
   $2 [USB splitter][usb-splitter-cable] cable which injects extra
   power (two males which connect to USB power and the Orange Pi plus
   a single female port which takes the HDD USB plug).

4. USB power brick with at least two ports (I accumulated a few over
   years, otherwise it's maybe another $10).

<a
href="https://www.flickr.com/photos/nothingpersonal/29040829177/in/datetaken/"
title="Orange Pi Zero Syncthing node"><img
src="https://farm2.staticflickr.com/1778/29040829177_69b3fbc653.jpg"
alt="Orange Pi Zero Syncthing node"></a>

Syncthing is easily installed on Orange Pi with Armbian via apt which I
[automated with Ansible][syncthing-playbook]. Folder synchronisation
needs to be set up via Syncthing UI.

The whole thing fits anywhere in your furniture. It's probably better
to invest in multiple nodes rather than a single box. In my flat
resiliency means one node is under the bed and another is on top the
wardrobe. You can simply buy more houses to achieve truly global
resiliency for your file storage network.

Total budget: 30$ plus several hours of tinkering. Satisfaction:
maximum.

[syncthing]: https://syncthing.net/

[android]: https://play.google.com/store/apps/details?id=com.nutomic.syncthingandroid

[opi]: http://dzhus.org/en/posts/2017-09-10-orangepi.html

[syncthing-playbook]: https://github.com/dzhus/globalchypre/blob/44df280/playbooks/syncthing.yml

[usb-splitter-cable]: https://www.aliexpress.com/item/USB-2-0-A-Male-to-USB-Female-2-Double-Dual-USB-Female-Splitter-Extension-Cable/32828176439.html
