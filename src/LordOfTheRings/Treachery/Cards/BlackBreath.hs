module LordOfTheRings.Treachery.Cards.BlackBreath (blackBreath) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype BlackBreath = BlackBreath TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Peril.
blackBreath :: TreacheryCard BlackBreath
blackBreath = treachery BlackBreath Cards.blackBreath

-- | Counts as a Condition attachment with the text: "Limit 1 per hero. Reduce attached
-- hero's willpower, attack, and defense to 0."
instance HasModifiersFor BlackBreath where
  getModifiersFor (BlackBreath a) = withHost a \host -> modified a host [WillpowerReducedToZero, DoesNotCountAttack, DoesNotCountDefense]

-- | When Revealed: Attach to a hero you control, one without a copy (limit 1 per hero).
-- "You" is the player who revealed it, who chooses on his own (Peril).
instance RunMessage BlackBreath where
  runMessage msg t@(BlackBreath attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      you <- getRevealer
      heroes <- select (HeroControlledBy $ PlayerWithId you)
      free <- filterM (fmap null . treacheriesOn attrs.treacheryCardCode . toTarget) heroes
      chooseOrRunOneM you do
        questionLabeled "blackBreath.attach"
        causedBy attrs
        targets free \h -> attachTreachery attrs.treacheryId h
      pure t
    _ -> BlackBreath <$> runMessage msg attrs
