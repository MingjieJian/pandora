      subroutine WELLA
     $(G,A,PHI,WNX,EXT,XFAC)
C
C     Rudolf Loeser, 1989 Feb 27
C---- Dumps, for BELLA.
C     (This is version 2 of WELLA.)
C     !DASH
      save
C     !DASH
      real*8 A, EXT, G, PHI, WNX, XFAC
      integer LUEO
C     !COM
C---- MOLONGA     as of 2003 Dec 02
      integer     I,JUD,L,KOD,M
      real*8      T,H,C1,C2,C3,C4,AX,SHFT,WLP,WLM,ABISO,ET,SUM
      logical     DPL,DMP,STT
C
      dimension   C1(2),C3(2),C4(2),WLP(2),WLM(2)
      common      /MOLONG1/ T,H,C1,C2,C3,C4,AX,SHFT,WLP,WLM,ABISO,
     $                      ET,SUM
      common      /MOLONG2/ I,JUD,L,KOD,M
      common      /MOLONG3/ DPL,DMP,STT
C     Intermediates for CO-opacity calculation.
C     .
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external LINER, HI, BYE
C
      call HI ('WELLA')
C     !BEG
      call LINER (1, LUEO)
C
      if(JUD.eq.+1) then
        write (LUEO,100) (I-1),I,(L-1),(L-1+KOD)
      else if(JUD.eq.-1) then
        write (LUEO,100) I,(I-1),(L-1),(L-1+KOD)
      end if
  100 format(' ','For j=',I3,' to j''=',I3,', v=',I3,' to v''=',I3)
C
      write (LUEO,101) AX,PHI,ABISO, WNX,EXT,XFAC, G,A
  101 format(' ',30X,'AX =',1PE20.12,5X,'PHI=',E20.12,
     $            5X,'ISO=',  E20.12/
     $       ' ',30X,'WNX=',  E20.12,5X,'EXT=',E20.12,
     $            5X,'FAC=',  E20.12/
     $       ' ',30X,'G  =',  E20.12,5X,'A  =',E20.12)
C     !END
      call BYE ('WELLA')
C
      return
      end
