module LordOfTheRings.Hero.Cards.Thranduil (thranduil) where

import LordOfTheRings.Card (CardType (AllyType))
import LordOfTheRings.Hero.CardDefs qualified as Cards
import LordOfTheRings.Hero.Import.Lifted
import LordOfTheRings.Phase (Phase (CombatPhase))
import LordOfTheRings.Trait (Trait (Silvan))

newtype Thranduil = Thranduil HeroAttrs
  deriving anyclass (IsHero, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

thranduil :: HeroCard Thranduil
thranduil = hero Thranduil Cards.thranduil

silvanAlly :: CardMatcher
silvanAlly = CardWithType AllyType <> CardWithTrait Silvan

-- | Combat Action: Play a Silvan ally from your hand. You do not need a resource match for
-- this ally. (Limit once per round.)
instance HasAbilities Thranduil where
  getAbilities (Thranduil a) =
    [ limited (PlayerLimit PerRound 1)
        $ restricted a 1 (DuringPhase CombatPhase <> AffordableAllyInHandWithoutMatch a.owner silvanAlly)
        $ ActionAbility Free
    ]

instance RunMessage Thranduil where
  runMessage msg h@(Thranduil attrs) = case msg of
    UseCardAbility pid (isSource attrs -> True) 1 _ -> do
      allies <- affordableAlliesInHandWithoutMatch pid silvanAlly
      chooseOrRunOneM pid do
        questionLabeled "thranduil.ally"
        causedBy attrs
        targets allies \c -> do
          phaseModifier attrs (CardIdTarget c.id) NoResourceMatchNeeded
          playFromHand pid c
      pure h
    _ -> Thranduil <$> runMessage msg attrs
