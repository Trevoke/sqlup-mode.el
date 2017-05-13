Feature: Upcasing SQL as I type in SQL interactive mode

  Background:
    Given the buffer is empty
    And I mock turn on sql-interactive-mode
    And I turn on sqlup-mode

  Scenario: Upcase normal sql keywords
    Given I type "select * from x"
    Then I should see "SELECT * FROM x"

  Scenario: keywords in strings and comments do not get upcased
    Given I type "select count(*) 'select' -- select "
    Then I should see "SELECT COUNT(*) 'select' -- select "

  Scenario: Typing newlines in the prompt should upcase strings
    When I type "select"
    When I press "RET" with missing buffer error ignored
    Then I should see "SELECT"
