# Acton Backend - Distributed Runtime & Database

The backend provides distributed computing capabilities, actor persistence, and fault tolerance for Acton applications.

## Quick Reference

### Directory Structure
```
backend/
├── actondb.c            # Main database implementation
├── db.c/h               # Core database operations
├── txns.c/h             # Transaction management
├── queue.c/h            # Message queue implementation
├── hash_ring.c/h        # Consistent hashing for distribution
├── comm.c/h             # Network communication
├── failure_detector/    # Node failure detection
├── build.zig            # Build configuration
└── test/                # Backend tests
```

### Build & Test
```bash
# Build backend (part of main build)
make

# Run backend tests
make test-backend

# Build standalone
cd backend && zig build
```

## Core Components

### ActonDB (`actondb.c`, `db.c/h`)

ActonDB is the distributed database that provides:
- Actor state persistence
- Transactional updates
- Replication across nodes
- Automatic failover

#### Key Features
- Write-ahead logging
- Snapshot isolation
- Multi-version concurrency control
- Consistent hashing for data distribution

#### API Overview
```c
// Initialize database
int actondb_init(const char *data_dir);

// Actor state operations
int actondb_put(actor_id_t actor, void *state, size_t size);
int actondb_get(actor_id_t actor, void **state, size_t *size);
int actondb_delete(actor_id_t actor);

// Transaction support
txn_t *actondb_txn_begin();
int actondb_txn_commit(txn_t *txn);
int actondb_txn_abort(txn_t *txn);
```

### Transaction System (`txns.c/h`, `txn_state.c/h`)

#### Transaction Lifecycle
```
BEGIN → ACTIVE → PREPARING → PREPARED → COMMITTING → COMMITTED
                     ↓                        ↓
                  ABORTING ← ← ← ← ← ← ← ABORTED
```

#### Implementation Details
- Two-phase commit protocol
- Distributed transaction coordination
- Conflict detection and resolution
- Deadlock prevention

### Message Queue System (`queue.c/h`, `queue_callback.c/h`)

#### Queue Types
1. **Actor Queues** - Per-actor message queues
2. **System Queues** - Internal system messages
3. **Remote Queues** - Inter-node communication

#### Features
- Lock-free implementation where possible
- Priority message handling
- Flow control
- Back-pressure mechanisms

### Distribution (`hash_ring.c/h`)

#### Consistent Hashing
- Maps actors to nodes
- Handles node additions/removals
- Minimizes data movement
- Virtual nodes for balance

#### Ring Operations
```c
// Add/remove nodes
hash_ring_add_node(ring, node_id, weight);
hash_ring_remove_node(ring, node_id);

// Find node for actor
node_id_t hash_ring_find_node(ring, actor_id);

// Get replica nodes
hash_ring_get_replicas(ring, actor_id, replicas, count);
```

### Communication Layer (`comm.c/h`)

#### Network Protocol
- Custom binary protocol
- Message framing
- Compression support
- Encryption ready

#### Message Types
```c
typedef enum {
    MSG_ACTOR_CALL,
    MSG_ACTOR_CAST,
    MSG_STATE_SYNC,
    MSG_HEARTBEAT,
    MSG_NODE_JOIN,
    MSG_NODE_LEAVE
} message_type_t;
```

### Failure Detection (`failure_detector/`)

#### Components
- **fd.c/h** - Main failure detector
- **cells.c/h** - Distributed state cells
- **vector_clock.c/h** - Logical time tracking

#### Detection Algorithm
- Adaptive timeout based on network conditions
- Gossip protocol for state dissemination
- Vector clocks for causality tracking
- Split-brain prevention

## Working with the Backend

### Adding New Database Operations

1. **Define Operation** in `db.h`:
```c
int actondb_new_operation(/* parameters */);
```

2. **Implement** in `db.c`:
```c
int actondb_new_operation(/* parameters */) {
    // Validate inputs
    // Begin transaction if needed
    // Perform operation
    // Handle errors
    // Return result
}
```

3. **Add Tests** in `test/db_unit_tests.c`

### Implementing New Message Types

1. **Define Message** in `comm.h`:
```c
typedef struct {
    message_header_t header;
    // Custom fields
} my_message_t;
```

2. **Add Handler** in `comm.c`:
```c
static void handle_my_message(connection_t *conn, 
                              my_message_t *msg) {
    // Process message
    // Send response if needed
}
```

3. **Register Handler**:
```c
message_handlers[MSG_MY_TYPE] = handle_my_message;
```

### Transaction Implementation

```c
// Example transactional operation
int perform_atomic_update(actor_id_t actor1, actor_id_t actor2) {
    txn_t *txn = actondb_txn_begin();
    if (!txn) return -1;
    
    // Read states
    void *state1, *state2;
    if (txn_get(txn, actor1, &state1) < 0) goto abort;
    if (txn_get(txn, actor2, &state2) < 0) goto abort;
    
    // Modify states
    modify_state(state1);
    modify_state(state2);
    
    // Write back
    if (txn_put(txn, actor1, state1) < 0) goto abort;
    if (txn_put(txn, actor2, state2) < 0) goto abort;
    
    // Commit
    return actondb_txn_commit(txn);
    
abort:
    actondb_txn_abort(txn);
    return -1;
}
```

## Performance Optimization

### Database Performance
- Use batch operations when possible
- Minimize transaction scope
- Leverage indexes appropriately
- Monitor lock contention

### Network Performance
- Batch messages when possible
- Use compression for large payloads
- Connection pooling
- Async I/O throughout

### Memory Management
- Pool allocators for hot paths
- Minimize allocations in critical sections
- Use stack allocation where appropriate
- Profile memory usage regularly

## Debugging & Monitoring

### Logging
```c
// Log levels
LOG_ERROR("Failed to connect: %s", error_msg);
LOG_WARN("Retry attempt %d", retry_count);
LOG_INFO("Node %s joined", node_id);
LOG_DEBUG("Message received: %d bytes", size);
```

### Metrics
- Transaction throughput
- Message queue depths
- Network latency
- Node health status

### Debug Tools
```bash
# Enable debug logging
export ACTON_LOG_LEVEL=debug

# Trace messages
export ACTON_TRACE_MESSAGES=1

# Database diagnostics
actondb-debug --stats
```

## Testing Strategy

### Unit Tests
- Test individual components
- Mock external dependencies
- Focus on edge cases

### Integration Tests
- Multi-node scenarios
- Failure injection
- Performance benchmarks

### Stress Tests
- High message rates
- Large state sizes
- Network partitions
- Node failures

## Security Considerations

1. **Authentication**
   - Node-to-node authentication
   - Message signing
   - Certificate management

2. **Encryption**
   - TLS for network communication
   - At-rest encryption for database

3. **Access Control**
   - Actor-level permissions
   - Operation authorization

## Common Issues & Solutions

### Issue: High Message Latency
- Check network conditions
- Verify queue depths
- Look for lock contention
- Consider batching

### Issue: Transaction Conflicts
- Reduce transaction scope
- Add retry logic
- Consider optimistic locking
- Review access patterns

### Issue: Memory Growth
- Check for queue buildup
- Verify state cleanup
- Look for memory leaks
- Monitor GC activity

## Future Considerations

The backend is designed to support:
- Geographic distribution
- Multi-datacenter deployment
- Elastic scaling
- Advanced replication strategies