# Hw3
Jake Armendariz

**1) 9.5 Describe the steps involved in looking up a pathname component.**
Components are lookedup one at at time, they are linked together, one component at a time until the component we are searching for is found. In this way the lookup can search through different filesystems (mount points). 


**2) 9.9 How can hard links be used to gain access to files that could not be accessed if a symbolic link were used instead?** 
If a hard link was used and a inode was modified by someone else, the hardlink will continue to point at the old data. A symbolic link on the other hand, would point at the updated/modified data.


**3) 9.22 Whats the difference between a logical and physical block, whats the difference?**  
Logical blocks don't have to worry about disk sectors, and allow the programmer to write their data one block at a time. Physical blocks may have different sizes and reference a specific location on disk in one or more contigious sectors.

**4) 9.30 What problem would arise if files had to be allocated contigiously on a piece of disk** A few problems would arise as in two cases, one a file growing larger, two a file with holes in it. Say a file is created in an empty space with 8192 of free memory (2 physical blocks), now if someone wrote to this file 20 KB, the location would have to be moved somewhere else, and then if it got bigger, again the data would have to be copied into a buffer, and moved to a larger place on disk. The problem only gets more complicated when files get holes in them and when a user wants to accesss a specific byte address of the file in which the hole exists.

This is obviously unpractical and extermely inefficient for writeable filesystems, so instead we use a pointer per block with indirect pointers to reduce filesize

(The contigous allocation still works well on read only filesystems, such as CDs)

**5) 10.2 List 5 problems that ZFS checksum can detect**

I found 6 in the textbook p525

1. Bit rot on disk
2. Phantom writes
3. Misdirected reads and writes
4. DMA Parity Errors
5. Bugs in disk drivers and disk firmware
6. Accidental overwrite of disk data

**6) 10.6 Why is ZFS checksum contained in its block pointer**
We keep the checksum in the dnode because if we read the wrong block the checksum won't match. The checksum should match the data directly to the pointer. If we kept the checksum with the data we wouldn't know when it was corrupted.
 

**7) Explain how a root user can be prevented from modifying critical system configuration files (such as those in /etc) while still providing superuser access to all of the files and directories in a portion of the file system tree. How does your answer relate to the relationship between implicit and explicit privilege?**

Well a super user implicitly has access to the entire system. And I am not sure exactly what this question is referring to I have two answers

1.  While the root is essentially the superuser, maybe we could limit the implicit access of the root user, while allowing it to have access to `sudo` and use sudo to access any file explicitly, this would limit root's access at first, but when it explicitly calls `sudo` can do anything.
2.  a union-fs a user could be able to edit the top layer of the filesystem while they leave the bottom layer untouched.


**8) In FreeBSD, file permissions reside in a file's inode. What differences in security and access control would there be if file permissions resided, instead, in the directory entry that pointed to a file? Explain situations in which this approach might produce a different result from the FreeBSD approach.**

One possibility is that entires files could be hidden from the user rather than just restricted. As the directory contains the list of files, this list could be configiured to keep file as unviewable and thus unaccessible to the user. In freebsd a user may be able to see a file exists, but not view it