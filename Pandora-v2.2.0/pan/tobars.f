      subroutine TOBARS
     $(M,ZCM,KZANX,Y,DYZ,GG,DGZ,S,CHK)
C
C     Rudolf Loeser, 1998 Apr 21
C---- Prints a check quantity, for diffusion.
C     !DASH
      save
C     !DASH
      real*8 CHK, DGZ, DYZ, GG, S, Y, ZCM
      integer I, J, KZANX, LUEO, M
      character BLANK*1, CC*13, CD*31, CS*13, MARK*1, STAR*1
C     !COM
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(45),STAR  )
      equivalence (SYMBS(43),BLANK )
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external LINER, NOTICE, SHIM, HI, BYE
C
C               J = N+MXTAP
C
C               ZCM(J), Y(J), DYZ(J), GG(J), DGZ(J), CHK(J), KZANX(N),
      dimension ZCM(*), Y(*), DYZ(*), GG(*), DGZ(*), CHK(*), KZANX(*),
C
C               S(J)
     $          S(*)
C     !EJECT
C
      call HI ('TOBARS')
C     !BEG
      call LINER    (2, LUEO)
      write (LUEO,100)
  100 format(' ','Verification of Special N1 solution.'//
     $       ' ','DYZ   = dy/dZ'/
     $       ' ','G     = g*y - f*DYZ'/
     $       ' ','DGZ   = dG/dZ'/
     $       ' ','check = DGZ + r*y ,  should equal s.'//
     $       ' ',19X,'Z(cm)',19X,'y',11X,'DYZ',13X,'G',11X,'DGZ',
     $           8X,'check',12X,'s',6X,'check : s')
      call LINER    (1, LUEO)
C
      J = 1
      do 102 I = 1,M
        MARK = BLANK
        if(I.eq.KZANX(J)) then
          MARK = STAR
          J    = J+1
        end if
        call NOTICE (13, CHK(I), S(I), CC, CS, CD)
        write (LUEO,101) I,MARK,ZCM(I),Y(I),DYZ(I),GG(I),DGZ(I),
     $                   CC,CS,CD
  101   format(' ',I4,A1,1P2E20.12,3E14.4,2A13,2X,A13)
        call SHIM   (I, 5, LUEO)
  102 continue
C     !END
      call BYE ('TOBARS')
C
      return
      end
