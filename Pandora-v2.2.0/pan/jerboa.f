      subroutine JERBOA
     $(IU,IL,K,N,NR,MPROM,CALLER)
C
C     Rudolf Loeser, 2000 May 03
C---- Prints error message for PEEK.
C     (This is version 2 of JERBOA.)
C     !DASH
      save
C     !DASH
      integer IL, IU, K, LUEO, MPROM, N, NR
      character CALLER*(*)
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external MESHED, MASHED, HI, BYE
C
      call HI ('JERBOA')
C     !BEG
      call MESHED (CALLER, 3)
      write (LUEO,100) IU,IL,K,N,NR,MPROM
  100 format(' ','MPROM does not make sense; profile analysis ',
     $           'will be skipped.'//
     $       ' ','(',I2,'/',I2,'); K =',I5,', N =',I8,' and NR =',I4,
     $           '; MPROM =',I12)
      call MASHED (CALLER)
C     !END
      call BYE ('JERBOA')
C
      return
      end
