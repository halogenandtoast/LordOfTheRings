module LordOfTheRings.Hero.Cards.Hirgon (hirgon) where

import LordOfTheRings.Card (Card (..), CardType (AllyType), costAsResources)
import LordOfTheRings.Hero.CardDefs qualified as Cards
import LordOfTheRings.Hero.Import.Lifted
import LordOfTheRings.Sphere (Sphere (Tactics))

newtype Hirgon = Hirgon HeroAttrs
  deriving anyclass (IsHero, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

hirgon :: HeroCard Hirgon
hirgon = hero Hirgon Cards.hirgon

-- | Response: After Hirgon quests successfully, play a tactics ally from your hand,
-- reducing its cost by 1 (to a minimum of 1). Then, you may raise your threat by 1 to give
-- that ally +1 attack and +1 defense until the end of the round.
instance HasAbilities Hirgon where
  getAbilities (Hirgon a) =
    [ restricted a 1 (exists (HeroWithId a.id <> CommittedHero) <> CardInHand a.owner AllyType)
        $ response
        $ QuestedSuccessfullyWindow After
    ]

instance RunMessage Hirgon where
  runMessage msg h@(Hirgon attrs) = case msg of
    UseCardAbility pid (isSource attrs -> True) 1 _ -> do
      player <- getPlayer pid
      tactics <- getResourcesOfSphere pid Tactics
      let cost c = maybe 0 costAsResources c.cost
          reduction c = if cost c > 1 then 1 else 0
      allies <-
        filterM passesUnique
          [c | c <- player.hand, c.cardType == AllyType, c.sphere == Just Tactics, tactics >= cost c - reduction c]
      chooseOneM pid do
        questionLabeled "hirgon.chooseAlly"
        causedBy attrs
        targets allies \c -> do
          when (reduction c > 0) $ phaseModifier attrs (CardIdTarget c.id) (CostModifier $ negate $ reduction c)
          push $ PlayCardInWindow pid c.id []
          chooseOneM pid do
            questionLabeled "hirgon.boost"
            causedBy attrs
            labeled "hirgon.raiseThreat" do
              raiseThreat pid 1
              roundModifiers attrs (CardIdTarget c.id) [AttackModifier 1, DefenseModifier 1]
            labeled "hirgon.decline" $ pure ()
      pure h
    _ -> Hirgon <$> runMessage msg attrs
