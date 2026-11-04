module LordOfTheRings.Event.Cards.TidesOfFate (tidesOfFate) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Sphere (Sphere (Tactics))

newtype TidesOfFate = TidesOfFate EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Response: When a shadow card increases an enemy's attack by any amount, increase the
-- defending character's defense by 3 for that attack. Then, the players as a group may
-- spend 2 tactics resources to ready the defending character and give it +3 attack for
-- its next attack this phase.
tidesOfFate :: EventCard TidesOfFate
tidesOfFate =
  eventWith TidesOfFate Cards.tidesOfFate \a ->
    a
      & windowL ?~ ShadowIncreasedAttackWindow When
      & criteriaL .~ CharactersAtLeast 1 DefendingHero DefendingAlly

instance RunMessage TidesOfFate where
  runMessage msg e@(TidesOfFate attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      defenders <- selectCharacters DefendingHero DefendingAlly
      for_ defenders \d -> attackModifier attrs d (DefenseModifier 3)
      tactics <- getGroupResourcesOfSphere Tactics
      when (tactics >= 2) $ chooseOneM pid do
        questionLabeled "tidesOfFate.spend"
        causedBy attrs
        labeled "tidesOfFate.ready" do
          spendGroupResources pid Tactics 2
          for_ defenders \d -> do
            ready d
            phaseModifier attrs d (ForNextAttack $ AttackModifier 3)
        labeled "tidesOfFate.done" (pure ())
      pure e
    _ -> TidesOfFate <$> runMessage msg attrs
