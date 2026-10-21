module LordOfTheRings.Ally.Cards.Rumil (rumil) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Keyword (Keyword (Ranged))

newtype Rumil = Rumil AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

rumil :: AllyCard Rumil
rumil = ally Rumil Cards.rumil

-- | Ranged. Response: After you play Rúmil from your hand, choose an enemy engaged with a
-- player. Deal X damage to that enemy where X is the number of ranged characters you
-- control.
instance HasAbilities Rumil where
  getAbilities (Rumil a) =
    [ restricted a 1 (exists $ EnemyEngagedWith AnyPlayer)
        $ response
        $ PlayedFromHandWindow After (CardIdTarget a.allyCardId)
    ]

instance RunMessage Rumil where
  runMessage msg s@(Rumil a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      let you = PlayerWithId pid
      x <- length <$> selectCharacters (HeroControlledBy you <> HeroWithKeyword Ranged) (AllyControlledBy you <> AllyWithKeyword Ranged)
      enemies <- select (EnemyEngagedWith AnyPlayer)
      chooseOneM pid do
        questionLabeled "chooseEnemy"
        causedBy a
        targets enemies \e -> dealDamage a e x
      pure s
    _ -> Rumil <$> runMessage msg a
