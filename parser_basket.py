# -*- coding: utf-8 -*-
"""
Created on Thu Sep 13 00:25:36 2018

@author: henri
"""
basketpath="df1_assoc_basket.txt"
filepath="df1_assoc.tsv"
cont=0
with open(basketpath,"w") as basket:
    with open(filepath,"r") as f:
        #skip the header
        f.readline() 
        for line in f:
            s=line.split()
            #admissao
            evento=1 #codigo da admissao
            attrs=[]
            for i in range(0,14):
                attr = s[i].strip('\"')
                if(i == 11):
                    prontuario = attr
                elif not attr.endswith("NA"):
                    attrs.append(attr)
            tam = len(attrs)
            linha = [prontuario,evento,tam] + attrs
            basket.write(" ".join(map(str,linha))+"\n")
            
            #1 aval e dieta
            evento=10 #codigo da 1 aval e dieta
            attrs=[]
            for i in range(14,88):
                attr = s[i].strip('\"')
                if not attr.endswith("NA"):
                    attrs.append(attr)
            tam = len(attrs)
            linha = [prontuario,evento,tam] + attrs
            basket.write(" ".join(map(str,linha))+"\n")
            
            #2 aval e dieta
            if(s[94].strip('\n"').endswith("5")):
                evento=20 #codigo da 2 aval e dieta
                attrs=[]
                for i in range(88,163):
                    if(i != 94 ):
                        attr = s[i].strip('\"')
                        if not attr.endswith("NA"):
                            attrs.append(attr)
                tam = len(attrs)
                linha = [prontuario,evento,tam] + attrs
                basket.write(" ".join(map(str,linha))+"\n")
            
            #desfecho
            evento=30 #codigo desfecho
            attrs=[]
            for i in range(163,170):
                attr = s[i].strip('\"')
                if not attr.endswith("NA"):
                    attrs.append(attr)
            tam = len(attrs)
            linha = [prontuario,evento,tam] + attrs
            basket.write(" ".join(map(str,linha))+"\n")
            
            
            cont += 1
print(cont)