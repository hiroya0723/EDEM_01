program main
    use MethodModule
    use DEMModule
    
    implicit none
    
    real(8), allocatable :: points(:,:)
    real(8), allocatable :: springs(:,:,:)
    real(8), allocatable :: x(:,:), u(:,:), d(:,:), d0(:,:)
    real(8), allocatable :: e_mat(:,:,:), F_all(:,:,:), F(:,:)
    integer, allocatable :: spring_flag(:,:)
    
    integer n, m
    
    points = readFile('point2.dat', 6)
    n = size(points, 1) !要素数
    
    springs = makeSpring('spring.dat', n)
    
    d0 = initD(points)
    
    x = initX(points)
    F = initF('condition.dat', n)
    allocate(u(n,3))
    allocate(d(n,n))
    allocate(spring_flag(n,n))
    allocate(e_mat(n,n,4))
    allocate(F_all(3,n,n))
    u = 0.0d0
    d = 0.0d0
    e_mat = 0.0d0
    !do m = 1, 1
    call updateF_all(x, u, points(:,4), e_mat, d, d0, spring_flag, springs, F_all)
    call makeF(F_all, F)
        !x = solve(F, points)
    !enddo
end program main