UPDATE audit_log
SET details = details #- '{user_name}'
                      #- '{user_email}'
                      #- '{user_phone}'
                      #- '{user_password_hash}'
WHERE event = 'user created';
