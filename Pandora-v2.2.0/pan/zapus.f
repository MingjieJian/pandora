      subroutine ZAPUS
     $(LIM,N,Z,VSB,G,GC,YBRC,SN,RHOSO,RHOST,CALLER)
C
C     Rudolf Loeser, 1986 Jul 30
C---- Dumps, for SKUA.
C     !DASH
      save
C     !DASH
      real*8 G, GC, RHOSO, RHOST, SN, VSB, YBRC, Z
      integer I, LIM, LUEO, N
      character CALLER*(*)
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external LINER, SHIM, MASHED, HI, BYE
C
C               Z(N), VSB(N), G(N), GC(N), YBRC(N), RHOST(N), RHOSO(N),
      dimension Z(*), VSB(*), G(*), GC(*), YBRC(*), RHOST(*), RHOSO(*),
C
C               SN(N)
     $          SN(*)
C
      call HI ('ZAPUS')
C     !BEG
      call LINER  (2, LUEO)
      write (LUEO,100)
  100 format(' ','Final results:'//
     $       ' ',3X,80X,6X,'Background',9X,'Sobolev',10X,'Static'/
     $       ' ',3X,15X,'Z',10X,'V(Sob)',15X,'G',14X,'GC',12X,'S(n)',
     $           12X,'Jbar',2(13X,'Rho'))
      call LINER  (1, LUEO)
C
      do 103 I = 1,N
        if(I.le.LIM) then
          write (LUEO,101) I,Z(I),VSB(I),G(I),GC(I),SN(I),YBRC(I),
     $                   RHOSO(I),RHOST(I)
  101     format(' ',I3,1P8E16.8)
        else
          write (LUEO,102) I,Z(I),VSB(I),SN(I),YBRC(I),RHOST(I)
  102     format(' ',I3,1P2E16.8,32X,2E16.8,16X,E16.8)
        end if
        call SHIM (I, 5, LUEO)
  103 continue
C
      call MASHED (CALLER)
C     !END
      call BYE ('ZAPUS')
C
      return
      end
