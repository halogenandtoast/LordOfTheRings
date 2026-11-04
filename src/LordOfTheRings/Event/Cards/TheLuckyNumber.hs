module LordOfTheRings.Event.Cards.TheLuckyNumber (theLuckyNumber) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Helpers.Character (getAttack, getDefense, getWillpower)
import LordOfTheRings.Helpers.Modifiers (immuneToSource)

newtype TheLuckyNumber = TheLuckyNumber EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Action: Choose a character in play (other than Bilbo Baggins). Add Bilbo Baggins' total
-- [willpower], [attack], and [defense] to that character's [willpower], [attack], and
-- [defense] respectively until the end of the phase.
--
-- His totals are read as the event resolves.
theLuckyNumber :: EventCard TheLuckyNumber
theLuckyNumber =
  eventWith TheLuckyNumber Cards.theLuckyNumber \a ->
    a & criteriaL .~ (exists bilbo <> CharactersAtLeast 1 (NotHero bilbo) AnyAlly)

bilbo :: HeroMatcher
bilbo = HeroWithTitle "Bilbo Baggins"

instance RunMessage TheLuckyNumber where
  runMessage msg e@(TheLuckyNumber attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      bilbos <- map toTarget <$> select bilbo
      for_ (headMay bilbos) \b -> do
        willpower <- getWillpower b
        attack <- getAttack b
        defense <- getDefense b
        -- one immune to player card effects, as Beorn, cannot be chosen
        characters <- filterM (fmap not . immuneToSource (toSource attrs)) =<< selectCharacters (NotHero bilbo) AnyAlly
        chooseOrRunOneM pid do
          questionLabeled "theLuckyNumber.chooseCharacter"
          causedBy attrs
          targets characters \c ->
            phaseModifiers attrs c [WillpowerModifier willpower, AttackModifier attack, DefenseModifier defense]
      pure e
    _ -> TheLuckyNumber <$> runMessage msg attrs
