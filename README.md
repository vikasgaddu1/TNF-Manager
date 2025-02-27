# Research Project Management System

A Shiny-based application for managing research projects, tracking decisions, milestones, and programming tasks.

## Features

### 1. Decision Log Management
- Track and manage project decisions
- Document questions, rationales, and impacts
- Assign owners and track resolution status
- Filter and search capabilities

### 2. Milestone Tracking
- Create and manage project milestones
- Track completion status
- Assign team members
- Monitor progress

### 3. User Management
- Role-based access control (admin/user)
- User activity tracking
- Team member assignment

### 4. Reporting Efforts
- Organize work by study, database release, and reporting effort
- Track multiple reporting efforts simultaneously
- Link decisions and milestones to specific reporting efforts

### 5. Comments System
- Add comments to programming tasks
- Track addressed/unaddressed comments
- Maintain discussion history

## Technology Stack

- R Shiny
- PostgreSQL/SQLite
- DT (DataTables)
- Pool (Database Connection Pooling)
- Shiny Feedback
- Other R packages

## Database Schema

Key tables include:
- users
- decision_logs
- milestones
- reporting_efforts
- comments
- programming_tracker

## Installation

1. Clone the repository
2. Install required R packages: 