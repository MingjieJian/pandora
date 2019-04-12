      subroutine CHIDE
     $(Z,THETA,PE,ZDCHI)
C
C     Rudolf Loeser, 1982 Jun 22
C---- Computes the lowering of the ionization potential.
C     !DASH
      save
C     !DASH
      real*8 FAC, PE, RP, THETA, Z, ZDCHI
C     !DASH
      external HI, BYE
C
      data FAC /4.84D-4/
C
      call HI ('CHIDE')
C     !BEG
      RP = sqrt(PE)
C
      ZDCHI = Z*FAC*THETA*RP
C     !END
      call BYE ('CHIDE')
C
      return
      end
