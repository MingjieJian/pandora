      subroutine LOBELIA
     $(HEADER,BRIEF)
C
C     Rudolf Loeser, 1988 Dec 06
C---- Encodes CO line description data.
C     !DASH
      save
C     !DASH
      integer J, K, KAK1, KAK2, KAK3, KP, KTYPE
      character BRIEF*10, C*1, HEADER*(*), PART*4, Q*1, S*1
C     !COM
C---- DWARF       as of 1997 Nov 19
      integer     KAKOD,KAKODS
      parameter   (KAKOD=4)
      dimension   KAKODS(KAKOD)
      common      /DWARF/ KAKODS
C     Continuum wavelength value type specification parameters.
C     (These parameters are packed and unpacked by "BET".)
C     .
      equivalence
     $(KAKODS( 1),KAK1 ),(KAKODS( 2),KAK2 ),(KAKODS( 3),KAK3 ),
     $(KAKODS( 4),KTYPE)
C     .
C     !DASH
      external HI, BYE
C
      call HI ('LOBELIA')
C     !BEG
      if(KTYPE.eq.21) then
        write (HEADER,100)
  100   format('CO-lines, cutoff')
        write (BRIEF ,101)
  101   format('CO, cutoff')
C     !EJECT
      else
        if(KAK2.ge.500) then
          K = KAK2-500
          Q = ','
        else
          K = KAK2
          Q = '-'
        end if
C
        if(KTYPE.eq.22) then
          KP = K
        else if(KTYPE.eq.17) then
          KP = K+1
        else if(KTYPE.eq.20) then
          KP = K+2
        else if(KTYPE.eq.23) then
          KP = K+3
        end if
C
        if(KAK1.ge.500) then
          J = KAK1-500
          S = 'R'
        else
          J = KAK1
          S = 'P'
        end if
C
        if(J.le.9) then
          write (PART,102) S,J
  102     format(A1,I1,2X)
        else if(J.le.99) then
          write (PART,103) S,J
  103     format(A1,I2,1X)
        else
          write (PART,104) S,J
  104     format(A1,I3)
        end if
C
        if(KAK3.eq.0) then
          C = '*'
        else
          C = ' '
        end if
C
        write (HEADER,105) KP,Q,K,PART,C
  105   format('CO-lines ',I2,A1,I2,A4,' ',A1)
        write (BRIEF ,106) KP,Q,K,PART
  106   format('&',I2,A1,I2,A4)
      end if
C     !END
      call BYE ('LOBELIA')
C
      return
      end
