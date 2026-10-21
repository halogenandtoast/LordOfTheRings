module LordOfTheRings.Hero.Cards.BoromirTactics (boromirTactics) where

import LordOfTheRings.Hero.CardDefs qualified as Cards
import LordOfTheRings.Hero.Import.Lifted

newtype BoromirTactics = BoromirTactics HeroAttrs
  deriving anyclass (IsHero, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

boromirTactics :: HeroCard BoromirTactics
boromirTactics = hero BoromirTactics Cards.boromirTactics

-- | Action: Raise your threat by 1 to ready Boromir. (Limit once per phase.)
-- Action: Discard Boromir to deal 2 damage to each enemy engaged with a single player.
instance HasAbilities BoromirTactics where
  getAbilities (BoromirTactics a) =
    [ limited (PlayerLimit PerPhase 1) $ restricted a 1 (exists $ HeroWithId a.id <> ExhaustedHero) $ ActionAbility (RaiseThreatCost 1)
    , restricted a 2 (exists $ EnemyEngagedWith AnyPlayer) $ ActionAbility Free
    ]

instance RunMessage BoromirTactics where
  runMessage msg h@(BoromirTactics attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      ready attrs
      pure h
    UseCardAbility pid (isSource attrs -> True) 2 _ -> do
      discard attrs
      players <- filterM (selectAny . EnemyEngagedWith . PlayerWithId) =<< getRemainingPlayers
      chooseOrRunOneM pid do
        questionLabeled "boromir.choosePlayer"
        causedBy attrs
        targets players \p -> do
          enemies <- select (EnemyEngagedWith $ PlayerWithId p)
          for_ enemies \e -> dealDamage attrs e 2
      pure h
    _ -> BoromirTactics <$> runMessage msg attrs
