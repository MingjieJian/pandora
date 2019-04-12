      subroutine NAGOR
     $(XLM,WLM,CP1,G1,N,TE,XNE,XNC,V,H1,BD1,PL,UL,VL,XL)
C
C     Rudolf Loeser, 2003 Jan 08
C---- Prints details, for ROWAN.
C     !DASH
      save
C     !DASH
      real*8 BD1, CP1, G1, H1, PL, TE, UL, V, VL, WLM, XL, XLM, XNC,
     $       XNE
      integer I, LUEO, N
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external DACAPO, MESHED, MASHED, DASHER, LINER, HI, BYE
C
C               TE(N), V(N), H1(N), BD1(N), PL(N), UL(N), VL(N), XL(N),
      dimension TE(*), V(*), H1(*), BD1(*), PL(*), UL(*), VL(*), XL(*),
C
C               XNE(N), XNC(N)
     $          XNE(*), XNC(*)
C
      call HI ('NAGOR')
C     !BEG
      call DACAPO (XLM)
      call MESHED ('NAGOR', 2)
      call LINER  (2, LUEO)
      call DASHER (LUEO)
      call LINER  (1, LUEO)
      write (LUEO,100) XLM,WLM,CP1,G1
  100 format(' ','Calculation of highest H Ly lines opacity at ',
     $           'LM (nominal) =',1PE24.16,' (actual =',E24.16,')'//
     $       ' ','CP1 =',E16.8,5X,'G1 =',E16.8//
     $       ' ',6X,'i',10X,'TE',10X,'NE',10X,'NC',10X,'VM',10X,'H1',
     $           9X,'BD1',11X,'U',10X,'V1',6X,'x-term',11X,'P')
C
      call LINER  (1, LUEO)
      write (LUEO,101) (I,TE(I),XNE(I),XNC(I),V(I),H1(I),BD1(I),UL(I),
     $                    VL(I),XL(I),PL(I),I=1,N)
  101 format(5(' ',I7,1P10E12.4/))
C
      call MASHED ('NAGOR')
C     !END
      call BYE ('NAGOR')
C
      return
      end
