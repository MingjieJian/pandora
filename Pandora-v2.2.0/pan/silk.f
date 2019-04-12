      subroutine SILK
     $(N,NMT,ITER,EIDIF,SAME,HND,XNR,ZHEL,RZM,AEL,ETA,ZME,XNEPR,XNE,
     $ ZRN)
C
C     Rudolf Loeser, 1979 Sep 28
C---- Prints NE iterations debug data.
C     !DASH
      save
C     !DASH
      real*8 AEL, AVE, EIDIF, ETA, HALF, HND, R, RZM, XNE, XNEPR, XNR,
     $       ZHEL, ZME, ZRN
      integer I, ITER, LUEO, N, NMT
      logical SAME
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT(12),HALF  )
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external LINER, DIVIDE, SHIM, ARROUT, MESHED, MASHED, HI, BYE
C
C               HND(N), XNR(N), RZM(N), AEL(N), ZRN(N), ZME(N), XNE(N),
      dimension HND(*), XNR(*), RZM(*), AEL(*), ZRN(*), ZME(*), XNE(*),
C
C               XNEPR(N), ZHEL(N), ETA(N,NMT)
     $          XNEPR(*), ZHEL(*), ETA(*)
C     !EJECT
C
      call HI ('SILK')
C     !BEG
      call MESHED   ('SILK', 2)
C
      write (LUEO,100) ITER,EIDIF,SAME
  100 format(' ','NE iteration',I3,5X,'EIDIF',1PE15.7,5X,L10//
     $       ' ',13X,'HND',10X,'XNR',9X,'NHEL',10X,'RZM',10X,'AEL',
     $           10X,'ZRN',10X,'ZME',5X,'Rel.Diff.',9X,'New NE')
      call LINER    (1, LUEO)
C
      do 102 I = 1,N
        AVE = HALF*(XNE(I)+XNEPR(I))
        call DIVIDE ((XNE(I)-XNEPR(I)), AVE, R)
        write (LUEO,101) I,HND(I),XNR(I),ZHEL(I),RZM(I),AEL(I),ZRN(I),
     $                   ZME(I),R,XNE(I)
  101   format(' ',I3,1P7E13.5,2E15.7)
        call SHIM   (I, 5, LUEO)
  102 continue
C
      if((ITER.le.1).or.SAME) then
        call ARROUT (LUEO, ETA, N, NMT, 'ETA')
      end if
C
      call MASHED   ('SILK')
C     !END
      call BYE ('SILK')
C
      return
      end
