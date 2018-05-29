/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Too!
! To change this license header, choose License Headers in Project Properties.
! To change this template file, choose Tools | Templates
! and open the template in the editor.
!

!     
! File:   main.f90
! Author: hiroya
!
! Created on 2018/05/27, 23:21
!
program main
    use MethodModule
    
    implicit none
    
    real(8), allocatable :: points(:,:)
 
    points = readCsv("point.csv", 3)
    
    call printArray2(points)
end program main
ls | Templates
 * and open the template in the editor.
 */

