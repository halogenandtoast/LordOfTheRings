module LordOfTheRings.Hero.Cards.ThorinStonehelm (thorinStonehelm) where

import LordOfTheRings.Hero.CardDefs qualified as Cards
import LordOfTheRings.Hero.Import.Lifted

newtype ThorinStonehelm = ThorinStonehelm HeroAttrs
  deriving anyclass (IsHero, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

thorinStonehelm :: HeroCard ThorinStonehelm
thorinStonehelm = hero ThorinStonehelm Cards.thorinStonehelm

-- | Response: After Thorin Stonehelm is declared as an attacker, discard the top card of
-- your deck to deal 1 damage to an enemy engaged with you.
instance HasAbilities ThorinStonehelm where
  getAbilities (ThorinStonehelm a) =
    [ restricted
        a
        1
        (PlayerExists (PlayerWithId a.owner <> PlayerWithCardsInDeck 1) <> exists (EnemyEngagedWith $ PlayerWithId a.owner))
        $ response
        $ AttackerDeclaredWindow After (Just $ toTarget a)
    ]

instance RunMessage ThorinStonehelm where
  runMessage msg h@(ThorinStonehelm attrs) = case msg of
    UseCardAbility pid (isSource attrs -> True) 1 _ -> do
      discardTopOfDeck pid 1
      enemies <- select (EnemyEngagedWith $ PlayerWithId pid)
      chooseOrRunOneM pid do
        questionLabeled "chooseEnemy"
        causedBy attrs
        targets enemies \e -> dealDamage attrs e 1
      pure h
    _ -> ThorinStonehelm <$> runMessage msg attrs
