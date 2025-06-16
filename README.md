# 🛡️ Contract Pausing with Roles

A Clarity smart contract demonstrating the **Circuit Breaker Pattern** with role-based access control. Admins can pause and unpause contract operations to prevent issues during emergencies or maintenance.

## 🚀 Features

- 🔐 **Role-based Access Control**: Owner and admin management system
- ⏸️ **Contract Pausing**: Emergency pause/unpause functionality  
- 💰 **Deposit/Withdraw**: Basic token operations that respect pause state
- 🚨 **Emergency Withdrawals**: Users can withdraw funds even when paused
- 📊 **Pause History**: Track all pause/unpause actions with timestamps
- 🔄 **Batch Operations**: Efficient batch deposit functionality
- 📈 **Balance Tracking**: Individual user balance management

## 🛠️ Core Functions

### Admin Management
- `add-admin(new-admin)` - Add new admin (admin only)
- `remove-admin(admin-to-remove)` - Remove admin (admin only, cannot remove owner)
- `is-admin(user)` - Check if user has admin privileges

### Contract Control
- `pause-contract()` - Pause all operations (admin only)
- `unpause-contract()` - Resume operations (admin only)
- `is-contract-paused()` - Check current pause state

### User Operations
- `deposit(amount)` - Deposit STX (blocked when paused)
- `withdraw(amount)` - Withdraw STX (blocked when paused)
- `emergency-withdraw()` - Withdraw all funds (only when pa

