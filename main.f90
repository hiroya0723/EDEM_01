program main
    use MethodModule
    use DEMModule
    
    implicit none
    
    real(8), allocatable :: points(:,:)
    real(8), allocatable :: springs(:,:,:)
    real(8), allocatable :: x(:,:), d(:,:)
    real(8), allocatable :: e_mat(:,:,:), f_mat(:,:,:)
    
    real(8) u
    integer n
    
    points = readFile('point.dat', 6)
    n = size(points, 1) !要素数
    springs = makeSpring('spring.dat', n)
    d = initD(points)
    x = initX(points)
    e_mat = 
    u = loop(points, springs, x, d)
end program main