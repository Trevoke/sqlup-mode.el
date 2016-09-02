Feature: Postgres product specific tweaks

  Background:
    Given the buffer is empty
    And I turn on sql-mode
    And I turn on sqlup-mode
    And I use the sql product "postgres"

 Scenario: Upcase a normal SQL keyword in a postgres-execute eval string
    And I type "execute 'select "
    Then I should see "EXECUTE 'SELECT "

 Scenario: Upcase a normal SQL keyword in a postgres-format eval string
    And I type "execute format('select "
    Then I should see "EXECUTE format('SELECT "

 Scenario: Normal strings are never upcased
    And I type "select 'case "
    Then I should see "SELECT 'case "

  Scenario: Postgres meta commands are not upcased
    When I type "\c "
    Then I should see "\c "
