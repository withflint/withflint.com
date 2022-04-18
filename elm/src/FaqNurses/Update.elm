module FaqNurses.Update exposing (init, update)

import FaqNurses.Types exposing (Faq, FormattedText(..), Model, Msg(..))
import Html.Attributes exposing (id)
import Return exposing (Return, singleton)


init : Return Msg Model
init =
    singleton
        { title = "FAQ for Internationally Educated Nurses - Flint"
        , heroTitle = "FAQ for Internationally Educated Nurses"
        , faqs = faqs
        }


update : Msg -> Model -> Return Msg Model
update msg model =
    case msg of
        ToggleVisibilty id ->
            let
                toggle : Faq -> Faq
                toggle faq =
                    if faq.id == id then
                        { faq | isVisible = not faq.isVisible }

                    else
                        faq
            in
            singleton { model | faqs = List.map toggle model.faqs }


faqs : List Faq
faqs =
    [ { question = "Who is this for?"
      , answer = [ Paragraph "International educated Registered Nurses (RN) with a willingness to relocate to the US permanently to work as a RN." ]
      , isVisible = True
      }
    , { question = "How does it work?"
      , answer = [ Paragraph "We work with US employers such as hospitals to source great nurses internationally. We work with these international nurses getting them licensed, arranging a new job and immigration to ultimately begin their new life in the US." ]
      , isVisible = False
      }
    , { question = "How do I apply?"
      , answer = [ Paragraph "To get started you can visit withflint.com and click the apply button." ]
      , isVisible = False
      }
    , { question = "What do I need to apply?"
      , answer = [ Paragraph "To apply you need:", ListItem "A valid nursing license in any country", ListItem "Nursing education from a program approved by the U.S. State Nursing Board", ListItem "At least 1 year of Registered Nursing experience in a hospital setting", ListItem "Good english and communication skills" ]
      , isVisible = False
      }
    , { question = "Do I have to sign a contract with Flint?"
      , answer = [ Paragraph "Yes, the agreement is a two way commitment. It legally binds us to follow through with our financial obligations and to follow through with the process. The agreement also ensures that you will follow through with the process as we will be investing significant financial resources into you." ]
      , isVisible = False
      }
    , { question = "How do I know I can trust Flint?"
      , answer = [ Paragraph "We provide you the opportunity to connect with our other customers who have gone before you and are working in the US today as RNs. Before starting we will sign agreements with you which legally binds us to our obligations. We are a registered US company and are subject to all the immigration and labor laws." ]
      , isVisible = False
      }
    , { question = "What will Flint pay for and what do I need to pay for?"
      , answer = [ Paragraph "We pay for virtually everything except small initial expenses. We need to ensure that the candidate will take this seriously and follow through. For example, we may ask you to cover the fees associated with registering for the NCLEX qualifying exam." ]
      , isVisible = False
      }
    , { question = "How does Flint make money?"
      , answer = [ Paragraph "The US employer funds the whole process and we take a portion of that for our services." ]
      , isVisible = False
      }
    , { question = "Can I bring my family with me?"
      , answer = [ Paragraph "Yes there are opportunities for you to bring spouses and dependents with you to begin your new life in the US. The opportunity varies depending on the immigration path you take. If you want to immigrate with your family make sure to include that in your profile when you are prompted." ]
      , isVisible = False
      }
    , { question = "What will my living arrangements be in the US."
      , answer = [ Paragraph "We do provide temporary accommodations such as a hotel or Airbnb for the first two weeks in the US or until your first paycheck. We help you find long term rental living accommodations just like many other Americans who cannot afford to buy a house." ]
      , isVisible = False
      }
    , { question = "How will I be paid?"
      , answer = [ Paragraph "You will be paid every two weeks from your employer unless otherwise stated in your contract." ]
      , isVisible = False
      }
    , { question = "Should I save money in advance?"
      , answer = [ Paragraph "It is recommended to save money in order to cover some expenses before you receive your first paycheck. We do cover basic expenses for you such as temporary accommodations and food in your first two weeks in the US or until your first paycheck." ]
      , isVisible = False
      }
    , { question = "How much support will I get throughout the process?"
      , answer = [ Paragraph "We will provide you guidance through the whole process sharing next steps along the whole journey. This includes licensing, immigration, and employment arrangements. We connect you with other nurses who are at a similar part of the journey in your cohort to support and encourage each other. We also provide you the opportunity to connect with other nurses who have gone before you." ]
      , isVisible = False
      }
    , { question = "What type of Registered Nurses (RNs) are you looking for?"
      , answer = [ Paragraph "Currently looking for RNs with bedside experience for hospital positions." ]
      , isVisible = False
      }
    , { question = "Do you employ positions other than Registered Nurses (RNs)?"
      , answer = [ Paragraph "Currently we only employ RNs." ]
      , isVisible = False
      }
    , { question = "What will help my chances of being selected?"
      , answer = [ Paragraph "The more qualified you are the better. We prefer more experience, better communication, and more nursing education. If you already have a US nursing license or some of the documents required for US licensing that is also a benefit. In addition nursing experience in the following areas are more desirable: Intensive Care, Critical Care, Medical-surgical, Emergency." ]
      , isVisible = False
      }
    , { question = "How long is the process?"
      , answer = [ Paragraph "The process length varies case to case and depends on many factors such as the visa and country of origin. In most cases you must be licensed before we can start the immigration process. The faster you can be licensed the quicker you can start working in the US. While we cannot guarantee immigration processing times, for a typical employment based immigration path we expect up to 2 years for the full licensing and immigration to start work in the US." ]
      , isVisible = False
      }
    , { question = "Where in the US will I work?"
      , answer = [ Paragraph "We work with many different types of hospitals and health centers all over the US that are looking for nurses. You will go through a matching and interview process with prospective employers before we start immigration so you know where you will be going. We consider your skills, experience, and goals when assessing which hospital is best suited to you. We consider both your preferences and the needs of the hospital to ensure that we make a match that works well for nurses and the facilities." ]
      , isVisible = False
      }
    , { question = "How long is the employment contract and what happens when it's complete?"
      , answer = [ Paragraph "A typical employment contract will be 3 years. The goal is for us to match you with the right employer for a good long term fit as this is what the employers want as well. We consider your skills, experience, and goals when assessing which hospital is best suited to you. We consider both your preferences and the needs of the hospital to ensure that we make a match that works well for nurses and the facilities. You will have opportunities to review the contract before committing." ]
      , isVisible = False
      }
    , { question = "What type of US visa would I be using?"
      , answer = [ Paragraph "This depends on your situation. The most popular visa process is the EB3 visa which is an employment based permanent resident visa. If you are Canadian or Mexican other options include a TN visa which is a temporary work visa. By filling out your profile we help identify alternative immigration paths that may work better for your specific situation." ]
      , isVisible = False
      }
    , { question = "What is the NCLEX exam?"
      , answer = [ Paragraph "In order to become licensed to work in the US as a RN, nurses must take and pass a qualifying exam called the NCLEX. In most cases you must pass the NCLEX qualifying exam before we can start the immigration process. If you've already passed it you won't need to take it again." ]
      , isVisible = False
      }
    , { question = "How should I prepare for the NCLEX?"
      , answer = [ Paragraph "We provide study materials and resources to help you prepare. We also connect you with other nurses who are also preparing for the test to support each other." ]
      , isVisible = False
      }
    , { question = "What happens if I fail the NCLEX in my first attempt?"
      , answer = [ Paragraph "Failed tests will prolong the process so it's important to do your best to pass the first time. You may take the test again in 45 days at the earliest. This is a rule set by the testing authority." ]
      , isVisible = False
      }
    , { question = "If I'm already in the immigration and licensing process can I work with Flint?"
      , answer = [ Paragraph "We can but it depends on the situation. If you have specific questions you can reach out to our team to find out more." ]
      , isVisible = False
      }
    , { question = "What is the level of English proficiency that is required?"
      , answer = [ Paragraph "Employers care that you can communicate effectively with staff and patients. They require you to be fluent in English, which can be demonstrated in the interviews and in the English test." ]
      , isVisible = False
      }
    , { question = "Will I need to take an English test?"
      , answer = [ Paragraph "Yes, for immigration and licensing you will need to take one of either the IELTS or TOEFL english tests unless your nursing education was in the following countries and your textbooks were in English (United Kingdom, Australia, Canada (except Quebec), New Zealand, Ireland or the United States). If you have already taken an English test recently, we can use those scores." ]
      , isVisible = False
      }
    , { question = "What is a passing score for the English test?"
      , answer = [ Paragraph "For the IELTS (Academic Format) the overall score must be at least 6.5 and the spoken band 7.0. For the TOEFL iBT you must have an overall score greater than 83 and 26 in the speaking section." ]
      , isVisible = False
      }
    , { question = "Will I need a passport?"
      , answer = [ Paragraph "Yes you and every family member coming with you will need a passport." ]
      , isVisible = False
      }
    , { question = "Will I be able to study and continue to specialize in certain nursing areas after I come?"
      , answer = [ Paragraph "The US has lots of programs to help you achieve your career goals. Be sure to let your prospective employer know what your goals are." ]
      , isVisible = False
      }
    ]
        -- Generating id for faq.id
        |> (\faqs_ -> List.map2 (\id faq -> Faq id faq.question faq.answer faq.isVisible) (List.range 0 (List.length faqs_)) faqs_)
