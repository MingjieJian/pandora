      integer NL,MXL,MXI,MXM,MXT
      character LABM*5,LABK*5,LABN*5,LABB*5,ION*3
      character DN*80,DB*80,DM*80,HN*80,HB*80,WORK*80
C
      dimension LABM(5), LABK(14), LABN(14), LABB(14), ION(14)
      dimension DN(100,16,14), DB(100,15,14), DM(100,5)
      dimension NL(14), HN(14), HB(14), WORK(100)
C
      common /ALL/ DN,DB,DM,HN,HB,WORK,LABM,LABK,LABN,LABB,ION
      common /LIM/ NL,MXL,MXT,MXI,MXM
