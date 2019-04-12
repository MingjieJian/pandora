      subroutine IPOH
     $(K,L)
C
C     Rudolf Loeser, 2002 Oct 04
C---- Encodes a value, for TAHAN.
C     !DASH
      save
C     !DASH
      integer K, M
      character L*1, TAB*1
C     !DASH
      external HI, BYE
C
      dimension TAB(9)
C
      data TAB /'1', '2', '3', '4', '5', '6', '7', '8', '9'/
C
      call HI ('IPOH')
C     !BEG
      M = K/10
      if((M.gt.0).and.(M.le.9)) then
        L = TAB(M)
      else if (K.ge.6) then
        L = 'a'
      else if(K.ge.3) then
        L = 'm'
      else if(K.ge.1) then
        L = 'z'
      else
        L = '*'
      end if
C     !END
      call BYE ('IPOH')
C
      return
      end
