module LordOfTheRings.Treachery.Cards.OnslaughtOfMordor (onslaughtOfMordor) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype OnslaughtOfMordor = OnslaughtOfMordor TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

onslaughtOfMordor :: TreacheryCard OnslaughtOfMordor
onslaughtOfMordor = treachery OnslaughtOfMordor Cards.onslaughtOfMordor

{- | When Revealed: Each player deals 1 damage to a questing character he controls. Until
the end of the phase, each damaged character gets -1 willpower and cannot be healed: each
character with damage once that damage is dealt.
Shadow: Choose a non-unique enemy in the staging area. Engage it and deal it a shadow card.
-}
instance RunMessage OnslaughtOfMordor where
  runMessage msg t@(OnslaughtOfMordor attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      eachPlayer \pid -> do
        questing <- selectCharacters (HeroControlledBy (PlayerWithId pid) <> CommittedHero) (AllyControlledBy (PlayerWithId pid) <> CommittedAlly)
        chooseOrRunOneM pid do
          questionLabeled "onslaughtOfMordor.damage"
          causedBy attrs
          targets questing \c -> dealDamage attrs c 1
      doStep 1 msg
      pure t
    DoStep 1 (WhenRevealed (isTarget attrs -> True)) -> do
      damaged <- damagedCharacters AnyHero AnyAlly
      for_ damaged \c -> phaseModifiers attrs c [WillpowerModifier (-1), CannotBeHealed]
      pure t
    ResolveShadowEffect cid _ pid _ | cid == attrs.cardId -> do
      enemies <- select (EnemyInStagingArea <> not_ UniqueEnemy)
      chooseOrRunOneM pid do
        questionLabeled "onslaughtOfMordor.engage"
        causedBy attrs
        targets enemies \e -> do
          engageEnemy pid e
          dealShadowCard e
      pure t
    _ -> OnslaughtOfMordor <$> runMessage msg attrs
