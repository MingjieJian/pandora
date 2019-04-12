      subroutine ANGARA
     $(CALLER,N,K,IU,IL,ABC,SNU,H)
C
C     Rudolf Loeser, 1985 Jan 23
C---- Extra printout for TOBOL.
C     (This is version 2 of ANGARA.)
C     !DASH
      save
C     !DASH
      real*8 ABC, H, SNU
      integer IL, IU, K, LUEO, N
      character CALLER*(*), TITLE*31
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external MESHED, ARROUT, MASHED, HI, BYE
 
C               SNU(N,K), ABC(N,K), H(N,K)
      dimension SNU(*),   ABC(*),   H(*)
C
      call HI ('ANGARA')
C     !BEG
      call MESHED (CALLER, 2)
C
      write (TITLE,100) IU,IL
  100 format(' (',I2,'/',I2,') Source Function')
      call ARROUT (LUEO, SNU, N, K, TITLE)
C
      write (TITLE,101) IU,IL
  101 format(' (',I2,'/',I2,') Absorption Coefficient')
      call ARROUT (LUEO, ABC, N, K, TITLE)
C
      write (TITLE,102) IU,IL
  102 format(' (',I2,'/',I2,') Flux Distribution')
      call ARROUT (LUEO, H,   N, K, TITLE)
C
      call MASHED (CALLER)
C     !END
      call BYE ('ANGARA')
C
      return
      end
